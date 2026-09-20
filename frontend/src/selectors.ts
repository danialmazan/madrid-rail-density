import type { AppState, CoverageRow, Measure } from './types';

export function reduceState<K extends keyof AppState>(state:AppState,key:K,value:AppState[K]):AppState {
  return {...state,[key]:value};
}

export function displayedMedianCrossing(rows:CoverageRow[], exactMedian:number):number|null {
  if (exactMedian > 1500) return null;
  return rows.filter(row=>row.population_share>=.5).sort((a,b)=>a.radius_m-b.radius_m)[0]?.radius_m??null;
}

export function formatWeightedValue(value:number, measure:Measure, language:'en'|'es'):string {
  if (measure==='share') return new Intl.NumberFormat(language==='es'?'es-ES':'en-GB',{style:'percent',maximumFractionDigits:1}).format(value);
  return Math.round(value).toLocaleString(language==='es'?'es-ES':'en-GB');
}
