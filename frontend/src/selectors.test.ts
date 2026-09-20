import { describe, expect, it } from 'vitest';
import { displayedMedianCrossing, formatWeightedValue, reduceState } from './selectors';
import type { AppState, CoverageRow } from './types';

const row=(radius_m:number,population_share:number):CoverageRow=>({
  geography_type:'region',geography_id:'region',geography_name:'Comunidad de Madrid',point_method:'entrance',rail_network:'all_rail',radius_m,
  population:population_share*1000,population_share,geography_population:1000
});

describe('weighted chart selectors',()=>{
  it('updates shared state without mutating the previous state',()=>{
    const state={geography:'region'} as AppState;
    const next=reduceState(state,'geography','city');
    expect(next.geography).toBe('city');
    expect(state.geography).toBe('region');
  });
  it('maps an exact median to the first 25 metre cumulative crossing',()=>{
    expect(displayedMedianCrossing([row(300,.49),row(325,.51)],319.9)).toBe(325);
  });
  it('does not invent a crossing outside the published domain',()=>{
    expect(displayedMedianCrossing([row(1500,.4)],1700)).toBeNull();
  });
  it('formats weighted counts and shares in both locales',()=>{
    expect(formatWeightedValue(.512,'share','en')).toBe('51.2%');
    expect(formatWeightedValue(12345.4,'population','es')).toMatch(/12[.\s]345/);
  });
});
