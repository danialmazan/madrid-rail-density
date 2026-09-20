import * as maplibregl from 'maplibre-gl';
import type { Map as MapLibreMap, Marker as MapLibreMarker, Popup, StyleSpecification } from 'maplibre-gl';
import { Protocol } from 'pmtiles';
import type { AppState, PointMethod, RailData } from './types';
import { RAIL_COLORS, primaryMode } from './theme';
import { stationName } from './labels';

const distanceColors=['#7e5360','#db5d4a','#e5925f','#d1bd6a','#80a796','#3f7566','#14352b'];
maplibregl.setWorkerUrl(new URL('assets/maplibre-gl-worker.mjs',location.href).href);
let protocolReady=false;
function registerProtocol(){if(protocolReady)return;const protocol=new Protocol({metadata:true});maplibregl.addProtocol('pmtiles',protocol.tilev4);protocolReady=true;}
const assetUrl=(path:string)=>new URL(path,location.href).href;
const pmtilesUrl=(name:string)=>`pmtiles://${assetUrl(`tiles/${name}`)}`;
const bounds=(b:number[])=>[[b[0],b[1]],[b[2],b[3]]] as [[number,number],[number,number]];
const bandPaint=(property:string)=>['match',['get',property],0,distanceColors[0],1,distanceColors[1],2,distanceColors[2],3,distanceColors[3],4,distanceColors[4],5,distanceColors[5],distanceColors[6]] as maplibregl.ExpressionSpecification;
const primaryModesPaint=['match',['get','modes'],'metro',RAIL_COLORS.metro,'metro|cercanias',RAIL_COLORS.metro,'metro|metro_ligero',RAIL_COLORS.metro,'cercanias',RAIL_COLORS.cercanias,'cercanias|tranvia_parla',RAIL_COLORS.cercanias,'metro_ligero',RAIL_COLORS.metro_ligero,'metro_ligero|cercanias',RAIL_COLORS.metro_ligero,'tranvia_parla',RAIL_COLORS.tranvia_parla,RAIL_COLORS.all_rail] as maplibregl.ExpressionSpecification;
const modePaint=['case',['==',['get','point_method'],'entrance'],primaryModesPaint,['match',['get','source_mode'],'metro',RAIL_COLORS.metro,'cercanias',RAIL_COLORS.cercanias,'metro_ligero',RAIL_COLORS.metro_ligero,'tranvia_parla',RAIL_COLORS.tranvia_parla,primaryModesPaint]] as maplibregl.ExpressionSpecification;
const quietStyle:StyleSpecification={version:8,sources:{},layers:[{id:'paper',type:'background',paint:{'background-color':'#ebe9e2'}}]};
function residentialStyle():StyleSpecification{return {version:8,sources:{components:{type:'vector',url:pmtilesUrl('components.pmtiles')},rail:{type:'vector',url:pmtilesUrl('stations.pmtiles')}},layers:[
  {id:'paper',type:'background',paint:{'background-color':'#ebe9e2'}},
  {id:'residential-components',type:'fill',source:'components','source-layer':'components',minzoom:11,paint:{'fill-color':bandPaint('e_band'),'fill-opacity':['interpolate',['linear'],['zoom'],11,.55,12,.82,16,.94],'fill-outline-color':['interpolate',['linear'],['zoom'],12,'rgba(20,53,43,.08)',16,'rgba(20,53,43,.58)']}},
  {id:'connection-lines',type:'line',source:'rail','source-layer':'connections',minzoom:5,filter:['==',['get','station_id'],'__none__'],paint:{'line-color':modePaint,'line-width':1.3,'line-opacity':.65}},
  {id:'origin-points',type:'circle',source:'rail','source-layer':'origins',minzoom:5,filter:['==',['get','point_method'],'entrance'],paint:{'circle-radius':['interpolate',['linear'],['zoom'],5,2,16,3.5],'circle-color':'#fff','circle-stroke-color':modePaint,'circle-stroke-width':1.5,'circle-opacity':['step',['zoom'],0,12,1],'circle-stroke-opacity':['step',['zoom'],0,12,1]}}
]};}
function addOpenFreeMapBase(map:MapLibreMap,before?:string){
  if(map.getSource('ofm-vector'))return;
  map.addSource('ofm-natural',{type:'raster',tiles:['https://tiles.openfreemap.org/natural_earth/ne2sr/{z}/{x}/{y}.png'],tileSize:256,maxzoom:6,attribution:'<a href="https://openfreemap.org">OpenFreeMap</a> · © OpenMapTiles · © OpenStreetMap'});
  map.addSource('ofm-vector',{type:'vector',tiles:['https://tiles.openfreemap.org/planet/20260913_164504_pt/{z}/{x}/{y}.pbf'],minzoom:0,maxzoom:14});
  const layers:maplibregl.LayerSpecification[]=[
    {id:'ofm-natural',type:'raster',source:'ofm-natural',maxzoom:7,paint:{'raster-opacity':.28,'raster-saturation':-.8}},
    {id:'ofm-residential',type:'fill',source:'ofm-vector','source-layer':'landuse',filter:['==',['get','class'],'residential'],paint:{'fill-color':'#dedfd8','fill-opacity':.55}},
    {id:'ofm-parks',type:'fill',source:'ofm-vector','source-layer':'park',paint:{'fill-color':'#d8dfd6','fill-opacity':.72}},
    {id:'ofm-water',type:'fill',source:'ofm-vector','source-layer':'water',paint:{'fill-color':'#c8d4d3'}},
    {id:'ofm-buildings',type:'fill',source:'ofm-vector','source-layer':'building',minzoom:12,paint:{'fill-color':'#d6d4cd','fill-outline-color':'#c8c6bf'}},
    {id:'ofm-roads',type:'line',source:'ofm-vector','source-layer':'transportation',minzoom:7,filter:['match',['get','class'],['motorway','trunk','primary','secondary','tertiary'],true,false],paint:{'line-color':'#fff','line-opacity':.82,'line-width':['interpolate',['linear'],['zoom'],7,.4,15,3]}},
    {id:'ofm-rail',type:'line',source:'ofm-vector','source-layer':'transportation',filter:['==',['get','class'],'rail'],paint:{'line-color':'#8d8e89','line-width':1.2}}
  ];layers.forEach(layer=>map.addLayer(layer,before));
}

export class ResidentialMap {
  map:MapLibreMap;method:PointMethod='entrance';selected:string|null=null;popup:Popup|null=null;originsDismissed=false;
  constructor(private data:RailData,private onSelect:(id:string|null)=>void){
    const view=this.defaultView('region');registerProtocol();this.map=new maplibregl.Map({container:'residential-map',style:residentialStyle(),center:view.center,zoom:view.zoom,attributionControl:false});
    this.map.on('error',event=>console.error('Residential map:',event.error));this.map.addControl(new maplibregl.NavigationControl({showCompass:false}),'top-right');this.map.addControl(new maplibregl.FullscreenControl(),'top-right');this.map.addControl(new maplibregl.AttributionControl({compact:true}),'bottom-right');
    this.map.on('click',()=>this.clearSelection(true));this.map.on('zoomend',()=>{if(this.originsDismissed){this.originsDismissed=false;this.applyFilters();}});window.addEventListener('scroll',()=>this.popup?.remove(),{passive:true});
    data.stations.forEach(station=>{const element=document.createElement('button'),name=stationName(station.station_name),color=RAIL_COLORS[primaryMode(station.modes)]??RAIL_COLORS.all_rail;element.className='station-anchor';element.dataset.stationId=station.station_id;element.title=name;element.setAttribute('aria-label',name);element.style.setProperty('--station-color',color);element.addEventListener('click',event=>{event.stopPropagation();if(this.selected===station.station_id){this.clearSelection(true);return;}this.showSelection(station.station_id);this.popup=new maplibregl.Popup({closeButton:false,closeOnClick:false,offset:12,className:'station-name-popup'}).setLngLat([station.longitude,station.latitude]).setText(name).addTo(this.map);this.onSelect(station.station_id);});new maplibregl.Marker({element,anchor:'center'}).setLngLat([station.longitude,station.latitude]).addTo(this.map);});
    this.map.once('style.load',()=>{addOpenFreeMapBase(this.map,'residential-components');this.applyFilters();});
  }
  private defaultView(geography:'region'|'city'){return {center:[-3.7038,40.4168] as [number,number],zoom:geography==='city'?12.4:12};}
  private applyFilters(){if(!this.map.getLayer('origin-points'))return;const originFilter=(this.originsDismissed?['==',['get','station_id'],'__none__']:this.selected?['all',['==',['get','point_method'],this.method],['==',['get','station_id'],this.selected]]:['==',['get','point_method'],this.method]) as maplibregl.FilterSpecification;const connectionFilter=(this.selected?['all',['==',['get','point_method'],this.method],['==',['get','station_id'],this.selected]]:['==',['get','station_id'],'__none__']) as maplibregl.FilterSpecification;this.map.setFilter('origin-points',originFilter);this.map.setFilter('connection-lines',connectionFilter);const opacity:number|maplibregl.ExpressionSpecification=this.selected?1:['step',['zoom'],0,12,1];this.map.setPaintProperty('origin-points','circle-opacity',opacity);this.map.setPaintProperty('origin-points','circle-stroke-opacity',opacity);}
  setMethod(method:PointMethod){this.method=method;if(!this.map.getLayer('residential-components'))return;const band=method==='entrance'?'e_band':'p_band';this.map.setPaintProperty('residential-components','fill-color',bandPaint(band));this.applyFilters();}
  showSelection(id:string){this.selected=id;this.originsDismissed=false;this.applyFilters();}
  clearSelection(dismissOrigins=false){if(!this.selected&&!this.popup&&!dismissOrigins)return;this.selected=null;this.originsDismissed=dismissOrigins;this.popup?.remove();this.popup=null;this.applyFilters();this.onSelect(null);}
  reset(geography:'region'|'city'){this.clearSelection(true);this.map.easeTo({...this.defaultView(geography),duration:650});}
}

export class StationMap {
  map:MapLibreMap;markers=new Map<string,{marker:MapLibreMarker;element:HTMLButtonElement}>();selected='';
  constructor(private data:RailData,private onSelect:(id:string)=>void){
    this.map=new maplibregl.Map({container:'station-map',style:quietStyle,bounds:bounds(data.map.region_bbox),fitBoundsOptions:{padding:25},attributionControl:false});this.map.on('error',event=>console.error('Station map:',event.error));this.map.addControl(new maplibregl.NavigationControl({showCompass:false}),'top-right');this.map.addControl(new maplibregl.FullscreenControl(),'top-right');this.map.addControl(new maplibregl.AttributionControl({compact:true}),'bottom-right');
    data.stations.forEach(station=>{const element=document.createElement('button'),name=stationName(station.station_name);element.className='station-value-marker';element.dataset.stationId=station.station_id;element.title=name;element.setAttribute('aria-label',name);element.style.setProperty('--station-color',RAIL_COLORS[primaryMode(station.modes)]??RAIL_COLORS.all_rail);element.addEventListener('click',event=>{event.stopPropagation();this.onSelect(station.station_id);});const marker=new maplibregl.Marker({element,anchor:'center'}).setLngLat([station.longitude,station.latitude]).addTo(this.map);this.markers.set(station.station_id,{marker,element});});
    this.map.once('style.load',()=>{addOpenFreeMapBase(this.map);this.map.addSource('station-origins',{type:'geojson',data:assetUrl('data/station-origins.geojson')});this.map.addLayer({id:'station-origins',type:'circle',source:'station-origins',minzoom:12,paint:{'circle-radius':['interpolate',['linear'],['zoom'],12,2.5,16,4.5],'circle-color':'#fff','circle-stroke-color':modePaint,'circle-stroke-width':1.5}});});
  }
  update(values:Map<string,number>,_geography:'region'|'city',selectedId:string){
    this.selected=selectedId;const max=Math.max(1,...values.values());this.markers.forEach(({element},id)=>{const value=values.get(id),size=value===undefined?5:6+18*Math.sqrt(value/max),selected=id===selectedId;element.style.width=`${selected?Math.max(16,size):size}px`;element.style.height=`${selected?Math.max(16,size):size}px`;element.style.opacity=selected?'1':value===undefined?'.2':'.76';element.classList.toggle('selected',selected);element.setAttribute('aria-label',`${element.title}: ${Math.round(value??0).toLocaleString()}`);});
  }
  focus(id:string){const station=this.data.stations.find(candidate=>candidate.station_id===id);if(station)this.map.easeTo({center:[station.longitude,station.latitude],zoom:Math.max(13,this.map.getZoom()),duration:700});}
}

export function renderStationCard(data:RailData,state:AppState){
  const host=document.querySelector<HTMLElement>('#station-card')!,station=data.stations.find(s=>s.station_id===state.selectedStationId)??data.stations[0],curve=data.station_curves.find(d=>d.station_id===station.station_id&&d.point_method===state.stationMethod&&d.radius_m===state.radius),hinterland=data.hinterlands.find(d=>d.station_id===station.station_id&&d.point_method===state.stationMethod),rank=state.geography==='city'?curve?.rank_city:curve?.rank_region,hRank=state.geography==='city'?hinterland?.rank_city:hinterland?.rank_region,es=state.language==='es',locale=es?'es-ES':'en-GB';
  const labels=es?{fallback:'Punto central oficial de respaldo: no hay una entrada independiente publicada.',selected:'Estación seleccionada',catchment:`Residentes a ${state.radius} m`,catchmentRank:'Posición por área',nearest:'Población de estación más cercana',hRank:'Posición por zona',median:'Distancia del residente',mean:'Mediana / media ponderada'}:{fallback:'Official central-point fallback: no separate entrance is published.',selected:'Selected station',catchment:`Residents at ${state.radius} m`,catchmentRank:'Catchment rank',nearest:'Nearest-station population',hRank:'Hinterland rank',median:'Resident distance',mean:'Median / weighted mean'};
  const distance=(value:number|undefined)=>Number.isFinite(value)?`${Math.round(value!)} m`:'—',marks=station.modes.split('|').map(mode=>`<img src="${assetUrl(`mode-marks/${mode}.svg`)}" alt="${mode.replaceAll('_',' ')}">`).join('');
  host.innerHTML=`${station.entrance_fallback&&state.stationMethod==='entrance'?`<p class="fallback-warning">${labels.fallback}</p>`:''}<p class="eyebrow">${labels.selected}</p><h3>${stationName(station.station_name)}</h3><p class="modes"><span class="mode-marks">${marks}</span>${station.lines.replaceAll('|',' · ')}</p><div class="station-card-grid"><div><span>${labels.catchment}</span><strong>${Math.round(curve?.population??0).toLocaleString(locale)}</strong><small>${labels.catchmentRank} · #${rank??'—'}</small></div><div><span>${labels.nearest}</span><strong>${Math.round(hinterland?.nearest_station_population??0).toLocaleString(locale)}</strong><small>${labels.hRank} · #${hRank??'—'}</small></div><div><span>${labels.median}</span><strong>${distance(hinterland?.median_distance_m)} / ${distance(hinterland?.mean_distance_m)}</strong><small>${labels.mean}</small></div></div>`;
}
