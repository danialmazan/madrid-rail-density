import * as d3 from 'd3';
import type { AppState, CoverageRow, HistogramRow, RailData, StationCurve } from './types';
import { ENTITY_DASHES, POINT_DASHES, RAIL_COLORS } from './theme';
import { stationName } from './labels';

const SERIES_COLORS=['#ed5948','#60a6d2','#d9ab45','#70ae87','#a882bd'];
const fmt=d3.format(',');
const pct=d3.format('.1%');

function networkLabel(network:string,language:AppState['language']){
  const labels:Record<string,[string,string]>={all_rail:['All rail','Todo el tren'],metro:['Metro','Metro'],metro_cercanias:['Metro + Cercanías','Metro + Cercanías'],cercanias:['Cercanías','Cercanías'],metro_ligero:['Metro Ligero','Metro Ligero'],tranvia_parla:['Tranvía de Parla','Tranvía de Parla'],mlt:['Metro Ligero / Tranvía','Metro Ligero / Tranvía']};
  return (labels[network]??[network,network])[language==='es'?1:0];
}
function methodLabel(method:string,language:AppState['language']){return method==='platform'?(language==='es'?'Andén':'Platform'):(language==='es'?'Entrada':'Entrance');}
function host(selector:string){const el=document.querySelector<HTMLElement>(selector);if(!el)throw new Error(`Missing ${selector}`);el.innerHTML='';return el;}
function tooltip(event:PointerEvent|MouseEvent|TouchEvent,html:string){
  const tip=document.querySelector<HTMLElement>('#tooltip')!,touch='touches' in event?event.touches[0]:undefined,point=touch??event as PointerEvent;
  tip.innerHTML=html;tip.hidden=false;tip.style.left=`${Math.max(8,Math.min(innerWidth-288,point.clientX+14))}px`;tip.style.top=`${Math.max(8,Math.min(innerHeight-tip.offsetHeight-8,point.clientY+14))}px`;
}
function hideTooltip(){document.querySelector<HTMLElement>('#tooltip')!.hidden=true;}
window.addEventListener('scroll',hideTooltip,{passive:true});
window.addEventListener('pointerdown',event=>{if(!(event.target as HTMLElement).closest('.chart'))hideTooltip();});
function base(selector:string,height=390){
  const el=host(selector),width=Math.max(560,el.clientWidth||760),m={top:16,right:24,bottom:48,left:68};
  const svg=d3.select(el).append('svg').attr('viewBox',`0 0 ${width} ${height}`).attr('aria-hidden','true');
  return {el,width,height,m,svg,innerW:width-m.left-m.right,innerH:height-m.top-m.bottom};
}
function axes(g:d3.Selection<SVGGElement,unknown,null,undefined>,x:d3.ScaleLinear<number,number>,y:d3.ScaleLinear<number,number>,innerW:number,innerH:number,yFormat:(v:number)=>string){
  g.append('g').attr('class','grid').call(d3.axisLeft(y).ticks(5).tickSize(-innerW).tickFormat(()=>'')).call(group=>group.selectAll('.tick').filter(d=>Number(d)===0).remove());
  g.append('g').attr('class','axis').attr('transform',`translate(0,${innerH})`).call(d3.axisBottom(x).tickValues([0,300,600,900,1200,1500]).tickFormat(v=>`${v}m`));
  g.append('g').attr('class','axis').call(d3.axisLeft(y).ticks(5).tickFormat(v=>yFormat(Number(v))));
}
function addLegend(el:HTMLElement,items:{label:string;color:string;dash?:string}[]){
  const legend=document.createElement('div');legend.className='chart-legend';legend.setAttribute('aria-label','Chart legend');
  legend.innerHTML=items.map(item=>`<span><i class="legend-sample ${item.dash==='3 3'?'dotted':item.dash?'dashed':''}" style="border-color:${item.color}"></i>${item.label}</span>`).join('');el.append(legend);
}
function nearestAt<T extends {radius_m:number}>(values:T[],radius:number){return values.reduce((best,row)=>Math.abs(row.radius_m-radius)<Math.abs(best.radius_m-radius)?row:best,values[0]);}
function cardRow(color:string,label:string,value:string,dash=''){return `<span class="tooltip-row"><i class="tooltip-series ${dash==='3 3'?'dotted':dash?'dashed':''}" style="border-color:${color}"></i><span>${label}</span><strong>${value}</strong></span>`;}
function lineInteraction<T extends {radius_m:number}>(g:d3.Selection<SVGGElement,unknown,null,undefined>,innerW:number,innerH:number,x:d3.ScaleLinear<number,number>,y:d3.ScaleLinear<number,number>,series:{values:T[];label:string;color:string;dash?:string;value:(row:T)=>number;format:(row:T)=>string}[]){
  const focus=g.append('g').attr('class','chart-focus').attr('visibility','hidden');focus.append('line').attr('class','focus-line').attr('y1',0).attr('y2',innerH);
  const dots=focus.selectAll('circle').data(series).enter().append('circle').attr('r',4.5).attr('fill',d=>d.color).attr('class','focus-dot');
  const show=(event:PointerEvent)=>{const [px]=d3.pointer(event),radius=Math.max(0,Math.min(1500,x.invert(px))),rows=series.map(item=>({item,row:nearestAt(item.values,radius)})),snapped=rows[0]?.row.radius_m??Math.round(radius);focus.attr('visibility','visible');focus.select<SVGLineElement>('.focus-line').attr('x1',x(snapped)).attr('x2',x(snapped));dots.attr('cx',x(snapped)).attr('cy',d=>y(d.value(nearestAt(d.values,snapped))));tooltip(event,`<strong>${Math.round(snapped)} m</strong>${rows.map(({item,row})=>cardRow(item.color,item.label,item.format(row),item.dash)).join('')}`);};
  g.append('rect').attr('class','chart-overlay').attr('x',0).attr('y',0).attr('width',innerW).attr('height',innerH).on('pointermove',show).on('pointerdown',show).on('pointerleave',()=>{focus.attr('visibility','hidden');hideTooltip();});
}

export function renderCoverage(data:RailData,state:AppState){
  const methods=state.coverageMethod==='compare'?['entrance','platform']:[state.coverageMethod],networks=state.railView==='all'?['all_rail']:['metro','metro_cercanias','all_rail'];
  const rows=data.coverage_curves.filter(d=>d.geography_id===state.geography&&methods.includes(d.point_method)&&networks.includes(d.rail_network)),groups=Array.from(d3.group(rows,d=>`${d.point_method}:${d.rail_network}`),([,values])=>values.sort((a,b)=>a.radius_m-b.radius_m));
  const {el,svg,m,innerW,innerH}=base('#coverage-chart',420),g=svg.append('g').attr('transform',`translate(${m.left},${m.top})`),x=d3.scaleLinear().domain([0,1500]).range([0,innerW]),value=(d:CoverageRow)=>state.areaMeasure==='share'?d.population_share:d.population,y=d3.scaleLinear().domain([0,state.areaMeasure==='share'?1:d3.max(rows,value)??1]).nice().range([innerH,0]);
  axes(g,x,y,innerW,innerH,state.areaMeasure==='share'?v=>pct(v):v=>d3.format('~s')(v));const line=d3.line<CoverageRow>().x(d=>x(d.radius_m)).y(d=>y(value(d)));
  groups.forEach(values=>g.append('path').datum(values).attr('class','chart-line').attr('stroke',RAIL_COLORS[values[0].rail_network]).attr('stroke-dasharray',POINT_DASHES[values[0].point_method]).attr('d',line));
  const series=groups.map(values=>({values,label:`${networkLabel(values[0].rail_network,state.language)} · ${methodLabel(values[0].point_method,state.language)}`,color:RAIL_COLORS[values[0].rail_network],dash:POINT_DASHES[values[0].point_method],value,format:(d:CoverageRow)=>state.areaMeasure==='share'?pct(d.population_share):`${fmt(d.population)} ${state.language==='es'?'residentes':'residents'}`}));
  lineInteraction(g,innerW,innerH,x,y,series);addLegend(el,series.map(({label,color},i)=>({label,color,dash:POINT_DASHES[groups[i][0].point_method]})));
}

export function renderDistribution(data:RailData,state:AppState){
  const methods=state.coverageMethod==='compare'?['entrance','platform']:[state.coverageMethod],networks=state.railView==='all'?['all_rail']:['metro','cercanias','mlt'];
  const rows=data.distance_histograms.filter(d=>d.geography_id===state.geography&&methods.includes(d.point_method)&&networks.includes(d.rail_network)&&!d.overflow&&d.band_start_m<1500&&d.band_end_m!==null),groups=Array.from(d3.group(rows,d=>`${d.point_method}:${d.rail_network}`),([,values])=>values.sort((a,b)=>a.band_start_m-b.band_start_m));
  const {el,svg,m,innerW,innerH}=base('#distribution-chart',420),g=svg.append('g').attr('transform',`translate(${m.left},${m.top})`),x=d3.scaleLinear().domain([0,1500]).range([0,innerW]),value=(d:HistogramRow)=>state.areaMeasure==='share'?d.population_share:d.population,y=d3.scaleLinear().domain([0,d3.max(rows,value)??1]).nice().range([innerH,0]);
  axes(g,x,y,innerW,innerH,state.areaMeasure==='share'?v=>pct(v):v=>d3.format('~s')(v));
  if(groups.length===1)g.selectAll('rect.band').data(rows).enter().append('rect').attr('class','band').attr('x',d=>x(d.band_start_m)+1).attr('y',d=>y(value(d))).attr('width',d=>Math.max(1,x(Math.min(1500,d.band_end_m!))-x(d.band_start_m)-2)).attr('height',d=>innerH-y(value(d))).attr('fill',RAIL_COLORS[rows[0].rail_network]).attr('opacity',.76);
  else groups.forEach(values=>{const points=values.flatMap(d=>[{x:d.band_start_m,row:d},{x:Math.min(1500,d.band_end_m!),row:d}]),line=d3.line<{x:number;row:HistogramRow}>().x(d=>x(d.x)).y(d=>y(value(d.row)));g.append('path').datum(points).attr('class','chart-line').attr('stroke',RAIL_COLORS[values[0].rail_network]).attr('stroke-dasharray',POINT_DASHES[values[0].point_method]).attr('d',line);});
  const focus=g.append('g').attr('class','chart-focus').attr('visibility','hidden');focus.append('line').attr('class','focus-line').attr('y1',0).attr('y2',innerH);const dots=focus.selectAll('circle').data(groups).enter().append('circle').attr('r',4.5).attr('class','focus-dot').attr('fill',d=>RAIL_COLORS[d[0].rail_network]);
  const show=(event:PointerEvent)=>{const [px]=d3.pointer(event),radius=Math.max(0,Math.min(1499.999,x.invert(px))),at=groups.map(values=>({values,row:values.find(d=>radius>=d.band_start_m&&radius<d.band_end_m!)??values.at(-1)!}));focus.attr('visibility','visible');focus.select<SVGLineElement>('.focus-line').attr('x1',x(radius)).attr('x2',x(radius));dots.attr('cx',x(radius)).attr('cy',values=>y(value(values.find(row=>radius>=row.band_start_m&&radius<row.band_end_m!)??values.at(-1)!)));const band=at[0]?.row;tooltip(event,`<strong>${band?.band_start_m}–${band?.band_end_m} m</strong>${at.map(({row})=>cardRow(RAIL_COLORS[row.rail_network],`${networkLabel(row.rail_network,state.language)} · ${methodLabel(row.point_method,state.language)}`,state.areaMeasure==='share'?pct(row.population_share):`${fmt(row.population)} ${state.language==='es'?'residentes':'residents'}`,POINT_DASHES[row.point_method])).join('')}`);};
  g.append('rect').attr('class','chart-overlay').attr('width',innerW).attr('height',innerH).on('pointermove',show).on('pointerdown',show).on('pointerleave',()=>{focus.attr('visibility','hidden');hideTooltip();});
  addLegend(el,groups.map(values=>({label:`${networkLabel(values[0].rail_network,state.language)} · ${methodLabel(values[0].point_method,state.language)}`,color:RAIL_COLORS[values[0].rail_network],dash:POINT_DASHES[values[0].point_method]})));
}

export function renderDistricts(data:RailData,state:AppState){
  const ids=state.districtIds.filter(Boolean),networks=state.districtRail==='all'?['all_rail']:['metro','cercanias','mlt'],rows=data.coverage_curves.filter(d=>ids.includes(d.geography_id)&&d.point_method===state.districtMethod&&networks.includes(d.rail_network)),groups=Array.from(d3.group(rows,d=>`${d.geography_id}:${d.rail_network}`),([,values])=>values.sort((a,b)=>a.radius_m-b.radius_m));
  const {el,svg,m,innerW,innerH}=base('#district-chart',510),g=svg.append('g').attr('transform',`translate(${m.left},${m.top})`),x=d3.scaleLinear().domain([0,1500]).range([0,innerW]),value=(d:CoverageRow)=>state.districtMeasure==='share'?d.population_share:d.population,y=d3.scaleLinear().domain([0,state.districtMeasure==='share'?1:d3.max(rows,value)??1]).nice().range([innerH,0]);
  axes(g,x,y,innerW,innerH,state.districtMeasure==='share'?v=>pct(v):v=>d3.format('~s')(v));const line=d3.line<CoverageRow>().x(d=>x(d.radius_m)).y(d=>y(value(d)));
  const series=groups.map(values=>{const districtIndex=ids.indexOf(values[0].geography_id),color=RAIL_COLORS[values[0].rail_network],dash=ENTITY_DASHES[districtIndex%ENTITY_DASHES.length],label=`${values[0].geography_name} - ${networkLabel(values[0].rail_network,state.language)}`;g.append('path').datum(values).attr('class','chart-line').attr('stroke',color).attr('stroke-dasharray',dash).attr('d',line);return {values,label,color,dash,value,format:(d:CoverageRow)=>state.districtMeasure==='share'?pct(d.population_share):`${fmt(d.population)} ${state.language==='es'?'residentes':'residents'}`};});
  lineInteraction(g,innerW,innerH,x,y,series);addLegend(el,series.map(({label,color,dash})=>({label,color,dash})));
}

export function renderStationCurves(data:RailData,state:AppState){
  const names=state.stationNames.filter(Boolean),rows=data.station_curves.filter(d=>names.includes(d.station_name)&&d.point_method===state.stationMethod),groups=Array.from(d3.group(rows,d=>d.station_name),([,values])=>values.sort((a,b)=>a.radius_m-b.radius_m));
  const {el,svg,m,innerW,innerH}=base('#station-chart',410),g=svg.append('g').attr('transform',`translate(${m.left},${m.top})`),x=d3.scaleLinear().domain([0,1500]).range([0,innerW]),value=(d:StationCurve)=>state.stationMeasure==='density'?(d.population_density_per_km2??0):d.population,y=d3.scaleLinear().domain([0,d3.max(rows,value)??1]).nice().range([innerH,0]);
  axes(g,x,y,innerW,innerH,v=>d3.format('~s')(v));const line=d3.line<StationCurve>().x(d=>x(d.radius_m)).y(d=>y(value(d)));
  const series=groups.map((values,i)=>{const color=SERIES_COLORS[i%SERIES_COLORS.length];if(i===0){const area=d3.area<StationCurve>().x(d=>x(d.radius_m)).y0(innerH).y1(d=>y(value(d)));g.append('path').datum(values).attr('class','chart-area').attr('fill',color).attr('d',area);}g.append('path').datum(values).attr('class','chart-line').attr('stroke',color).attr('d',line);return {values,label:stationName(values[0].station_name),color,dash:'',value,format:(d:StationCurve)=>fmt(value(d))};});
  lineInteraction(g,innerW,innerH,x,y,series);addLegend(el,series.map(({label,color})=>({label,color})));
}

export interface RankingDatum {station_id:string;station_name:string;modes:string;municipality_code:string;value:number}
export function rankingRows(data:RailData,state:AppState):RankingDatum[]{
  const allowed=(s:{municipality_code:string;modes:string})=>(state.geography==='region'||String(s.municipality_code).includes('28079'))&&(state.modeFilter==='all'||s.modes.split('|').includes(state.modeFilter));
  if(state.rankingMetric==='hinterland')return data.hinterlands.filter(d=>d.point_method===state.stationMethod&&allowed(d)).map(d=>({station_id:d.station_id,station_name:d.station_name,modes:d.modes,municipality_code:d.municipality_code,value:d.nearest_station_population}));
  return data.station_curves.filter(d=>d.point_method===state.stationMethod&&d.radius_m===state.radius&&allowed(d)).map(d=>({station_id:d.station_id,station_name:d.station_name,modes:d.modes,municipality_code:d.municipality_code,value:d.population}));
}
export function renderRanking(data:RailData,state:AppState,onSelect:(id:string)=>void){
  const rows=rankingRows(data,state).sort((a,b)=>state.direction==='top'?b.value-a.value:a.value-b.value).slice(0,15),{svg,m,innerH,innerW}=base('#ranking-chart',520);m.left=Math.min(190,Math.max(125,innerW*.3));const rankingW=innerW-(m.left-68),g=svg.append('g').attr('transform',`translate(${m.left},${m.top})`),labels=rows.map(d=>stationName(d.station_name)),y=d3.scaleBand().domain(labels).range([0,innerH]).padding(.28),x=d3.scaleLinear().domain([0,d3.max(rows,d=>d.value)??1]).nice().range([0,rankingW]);
  g.append('g').attr('class','axis').call(d3.axisLeft(y).tickSize(0));g.append('g').attr('class','grid').attr('transform',`translate(0,${innerH})`).call(d3.axisBottom(x).ticks(4).tickSize(-innerH).tickFormat(v=>d3.format('~s')(Number(v))));
  const show=(event:PointerEvent,d:RankingDatum)=>tooltip(event,`<strong>${stationName(d.station_name)}</strong>${cardRow(RAIL_COLORS[d.modes.split('|')[0]],state.language==='es'?'Residentes':'Residents',fmt(d.value))}`);
  g.selectAll('rect').data(rows).enter().append('rect').attr('x',0).attr('y',d=>y(stationName(d.station_name))!).attr('width',d=>x(d.value)).attr('height',y.bandwidth()).attr('fill',d=>d.station_id===state.selectedStationId?'#f3d36b':'#4d8373').attr('class',d=>d.station_id===state.selectedStationId?'ranking-bar selected':'ranking-bar').attr('tabindex',0).attr('role','button').on('click',(event,d)=>{show(event,d);onSelect(d.station_id);}).on('keydown',(event,d)=>{if(event.key==='Enter'||event.key===' '){event.preventDefault();onSelect(d.station_id);}}).on('pointermove',show).on('pointerleave',hideTooltip);
}
