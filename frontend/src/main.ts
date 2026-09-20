import './styles.css';
import { applyLanguage, translate } from './i18n';
import { renderCoverage, renderDistribution, renderDistricts, renderStationCurves, renderRanking, rankingRows } from './charts';
import { ResidentialMap, StationMap, renderStationCard } from './maps';
import type { AppState, RailData } from './types';
import { reduceState } from './selectors';
import { stationName } from './labels';

const qs=<T extends Element=HTMLElement>(s:string)=>document.querySelector<T>(s)!;
const qsa=<T extends Element=HTMLElement>(s:string)=>Array.from(document.querySelectorAll<T>(s));
const initialLanguage:AppState['language']=new URL(location.href).searchParams.get('lang')==='es'?'es':'en';
let state:AppState={language:initialLanguage,geography:'region',coverageMethod:'entrance',areaMeasure:'share',railView:'all',mapMethod:'entrance',districtMethod:'entrance',districtMeasure:'share',districtRail:'all',districtIds:['district_01','',''],stationMethod:'entrance',stationMeasure:'population',stationNames:['Atocha','',''],rankingMetric:'radius',radius:800,modeFilter:'all',direction:'top',selectedStationId:'crtm_interchange_054'};
let data:RailData,residentialMap:ResidentialMap,stationMap:StationMap;

function setPressed(selector:string,attribute:string,value:string){qsa<HTMLButtonElement>(selector).forEach(b=>b.setAttribute('aria-pressed',String(b.dataset[attribute]===value)));}
function bindButtons(selector:string,key:string,callback:(value:string)=>void){qsa<HTMLButtonElement>(selector).forEach(button=>button.addEventListener('click',()=>{setPressed(selector,key,button.dataset[key]!);callback(button.dataset[key]!);}));}
function renderHero(){const metric=data.area_metrics.find(d=>d.geography_id===state.geography&&d.point_method==='entrance'&&d.rail_network==='all_rail');qs('#hero-median').textContent=`${Math.round(metric?.median_distance_m??0)} m`;}
function renderArea(){renderHero();renderCoverage(data,state);renderDistribution(data,state);}
function selectedStationValues(){return new Map(rankingRows(data,state).map(d=>[d.station_id,d.value]));}
function renderStations(){renderStationCurves(data,state);renderRanking(data,state,id=>selectStation(id,true));stationMap?.update(selectedStationValues(),state.geography,state.selectedStationId);renderStationCard(data,state);}
function renderAll(){renderArea();renderDistricts(data,state);renderStations();}
function updateClearButtons(){qsa<HTMLButtonElement>('[data-clear-input]').forEach(button=>{const input=qs<HTMLInputElement>(`#${button.dataset.clearInput}`);button.hidden=!input.value.trim();});}
function selectStation(id:string,focusMap=false){state.selectedStationId=id;const station=data.stations.find(s=>s.station_id===id);if(station&&!state.stationNames.includes(station.station_name)){state.stationNames[0]=station.station_name;qs<HTMLInputElement>('#station-1').value=stationName(station.station_name);}updateClearButtons();renderStations();if(focusMap)stationMap?.focus(id);}

function populateControls(){
  const options=['<option value="">—</option>',...data.districts.slice().sort((a,b)=>a.district_name.localeCompare(b.district_name,'es')).map(d=>`<option value="${d.district_id}">${d.district_name}</option>`)].join('');
  ['#district-1','#district-2','#district-3'].forEach((id,i)=>{const select=qs<HTMLSelectElement>(id);select.innerHTML=options;select.value=state.districtIds[i];select.addEventListener('change',()=>{state.districtIds[i]=select.value;renderDistricts(data,state);});});
  qs('#district-1 option[value=""]').remove();
  const stations=data.stations.slice().sort((a,b)=>stationName(a.station_name).localeCompare(stationName(b.station_name),'es'));
  qs('#station-list').innerHTML=stations.map(s=>`<option value="${stationName(s.station_name)}"></option>`).join('');
  ['#station-1','#station-2','#station-3'].forEach((id,i)=>{const input=qs<HTMLInputElement>(id);input.addEventListener('input',updateClearButtons);input.addEventListener('change',()=>{const station=data.stations.find(s=>stationName(s.station_name)===input.value||s.station_name===input.value);state.stationNames[i]=station?.station_name??'';if(i===0&&station)state.selectedStationId=station.station_id;renderStations();updateClearButtons();});});
  qsa<HTMLButtonElement>('[data-clear-input]').forEach(button=>button.addEventListener('click',()=>{const input=qs<HTMLInputElement>(`#${button.dataset.clearInput}`);input.value='';state.stationNames[Number(input.id.slice(-1))-1]='';renderStations();updateClearButtons();input.focus();}));updateClearButtons();
  const labels=['0–200 m','200–400 m','400–600 m','600–800 m','800–1,000 m','1,000–1,500 m','> 1,500 m'],colors=['#7e5360','#db5d4a','#e5925f','#d1bd6a','#80a796','#3f7566','#14352b'];
  qs('#distance-legend').innerHTML=labels.map((label,i)=>`<span><i style="background:${colors[i]}"></i>${label}</span>`).join('');
}

function bindControls(){
  bindButtons('[data-geography]','geography',value=>{state=reduceState(state,'geography',value as AppState['geography']);residentialMap.reset(state.geography);renderAll();});
  bindButtons('[data-coverage-method]','coverageMethod',value=>{state=reduceState(state,'coverageMethod',value as AppState['coverageMethod']);renderArea();});
  bindButtons('[data-area-measure]','areaMeasure',value=>{state.areaMeasure=value as AppState['areaMeasure'];renderArea();});
  bindButtons('[data-rail-view]','railView',value=>{state.railView=value as AppState['railView'];renderArea();});
  bindButtons('[data-map-method]','mapMethod',value=>{state.mapMethod=value as AppState['mapMethod'];residentialMap.setMethod(state.mapMethod);});
  bindButtons('[data-district-method]','districtMethod',value=>{state.districtMethod=value as AppState['districtMethod'];renderDistricts(data,state);});
  bindButtons('[data-district-measure]','districtMeasure',value=>{state.districtMeasure=value as AppState['districtMeasure'];renderDistricts(data,state);});
  bindButtons('[data-district-rail]','districtRail',value=>{state.districtRail=value as AppState['districtRail'];renderDistricts(data,state);});
  bindButtons('[data-station-method]','stationMethod',value=>{state.stationMethod=value as AppState['stationMethod'];renderStations();});
  bindButtons('[data-station-measure]','stationMeasure',value=>{state.stationMeasure=value as AppState['stationMeasure'];renderStationCurves(data,state);});
  qsa<HTMLButtonElement>('[data-lang]').forEach(b=>b.addEventListener('click',()=>{state.language=b.dataset.lang as AppState['language'];applyLanguage(state.language);renderAll();const u=new URL(location.href);state.language==='es'?u.searchParams.set('lang','es'):u.searchParams.delete('lang');history.replaceState({},'',u);}));
  qs('#map-reset').addEventListener('click',()=>residentialMap.reset(state.geography));
  qs<HTMLSelectElement>('#ranking-metric').addEventListener('change',e=>{state.rankingMetric=(e.target as HTMLSelectElement).value as AppState['rankingMetric'];qs('#radius-label').toggleAttribute('hidden',state.rankingMetric==='hinterland');renderStations();});
  qs<HTMLInputElement>('#ranking-radius').addEventListener('input',e=>{state.radius=Number((e.target as HTMLInputElement).value);qs('#radius-value').textContent=`${state.radius} m`;renderStations();});
  qs<HTMLSelectElement>('#mode-filter').addEventListener('change',e=>{state.modeFilter=(e.target as HTMLSelectElement).value;renderStations();});
  qs<HTMLButtonElement>('#ranking-direction').addEventListener('click',e=>{const b=e.currentTarget as HTMLButtonElement;state.direction=state.direction==='top'?'bottom':'top';b.dataset.direction=state.direction;b.textContent=translate(state.language,state.direction);renderStations();});
}

async function init(){
  const response=await fetch('data/phase7.json');if(!response.ok)throw new Error(`Data request failed: ${response.status}`);data=await response.json() as RailData;
  populateControls();applyLanguage(state.language);bindControls();renderAll();
  residentialMap=new ResidentialMap(data,id=>{if(id)selectStation(id,false);});
  stationMap=new StationMap(data,id=>selectStation(id,true));
  (window as Window & {__railMaps?:{residential:ResidentialMap['map'];station:StationMap['map']}}).__railMaps={residential:residentialMap.map,station:stationMap.map};
  residentialMap.setMethod(state.mapMethod);
  stationMap.update(selectedStationValues(),state.geography,state.selectedStationId);
  const hero=qs('.hero');const updateStickyLanguage=()=>document.body.classList.toggle('geography-bar-active',hero.getBoundingClientRect().bottom<=1);updateStickyLanguage();window.addEventListener('scroll',updateStickyLanguage,{passive:true});
}
init().catch(error=>{console.error(error);document.body.insertAdjacentHTML('beforeend',`<p style="padding:2rem;color:#b00">The explorer could not load: ${String(error)}</p>`);});
