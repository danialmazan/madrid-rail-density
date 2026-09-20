#!/usr/bin/env node
import { open, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { PMTiles } from '../frontend/node_modules/pmtiles/dist/esm/index.js';

class NodeFileSource {
  constructor(path){this.path=path;}
  getKey(){return this.path;}
  async getBytes(offset,length){
    const handle=await open(this.path,'r');
    try {
      const buffer=Buffer.alloc(length);
      const {bytesRead}=await handle.read(buffer,0,length,offset);
      const view=buffer.subarray(0,bytesRead);
      return {data:view.buffer.slice(view.byteOffset,view.byteOffset+view.byteLength)};
    } finally {await handle.close();}
  }
}

const site=resolve(process.argv[2]??'site_phase7');
const output=resolve(process.argv[3]??'validation/phase7/pmtiles_validation.json');
const expected={
  components:{minZoom:12,maxZoom:16,layers:{components:{count:644613,attributes:['component_key','e_station','e_dist','e_band','p_station','p_dist','p_band']}}},
  surface:{minZoom:5,maxZoom:12,layers:{surface:{count:1582,attributes:['pop','e_dist','e_band','p_dist','p_band']}}},
  stations:{minZoom:5,maxZoom:16,layers:{stations:{count:357,attributes:['station_id','station_name','modes','lines']},origins:{count:2015,attributes:['station_id','origin_id','point_method','source_mode','source_record_id','is_fallback']},connections:{count:2015,attributes:['station_id','point_method']}}}
};

const archives={};
let passed=true;
for(const [name,contract] of Object.entries(expected)){
  const archive=new PMTiles(new NodeFileSource(resolve(site,'tiles',`${name}.pmtiles`)));
  const header=await archive.getHeader();
  const metadata=await archive.getMetadata();
  const layers=Object.fromEntries((metadata.tilestats?.layers??[]).map(layer=>[layer.layer,{count:layer.count,attributes:(layer.attributes??[]).map(attribute=>attribute.attribute).sort()}]));
  const layerChecks={};
  for(const [layerName,layerContract] of Object.entries(contract.layers)){
    const actual=layers[layerName];
    const attributesPresent=layerContract.attributes.every(attribute=>actual?.attributes.includes(attribute));
    layerChecks[layerName]={count:actual?.count??null,expectedCount:layerContract.count,attributesPresent,passed:actual?.count===layerContract.count&&attributesPresent};
    passed&&=layerChecks[layerName].passed;
  }
  const zoomsPass=header.minZoom===contract.minZoom&&header.maxZoom===contract.maxZoom;
  const nonempty=header.numAddressedTiles>0&&header.numTileEntries>0&&header.numTileContents>0;
  passed&&=zoomsPass&&nonempty;
  archives[name]={minZoom:header.minZoom,maxZoom:header.maxZoom,addressedTiles:header.numAddressedTiles,tileEntries:header.numTileEntries,tileContents:header.numTileContents,zoomsPass,nonempty,layers:layerChecks};
}
await writeFile(output,`${JSON.stringify({passed,archives},null,2)}\n`);
console.log(`PMTiles validation: ${passed?'PASS':'FAIL'} (${output})`);
if(!passed)process.exitCode=1;
