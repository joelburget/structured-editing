// @flow
import Sha from 'jssha/src/sha256';

import type { Term } from './types';

// TODO export this default
export type Address = string;

const store: { [key: string]: any } = {};

export function hashObj(obj: Object) {
  const json = JSON.stringify(obj);
  const sha = new Sha('SHA-256', 'TEXT');
  sha.update(json);
  const hash = sha.getHash('HEX');
  return { hash, json };
}

export function get(addr: Address): any {
  return store[addr];
}

export function set(val: Term<Address>): Address {
  const hash = val.hash;
  store[hash] = val;
  return hash;
}

type TermR = Term<TermR>;

function setRecursive_(val: TermR | Address): Address {
  return typeof val === 'string' ? val : setRecursive(val)[1];
}

export function setRecursive(val: TermR): [Term<Address>, Address] {
  const val_ = val.mapSubterms(setRecursive_);
  return [val_, set(val_)];
}

export function expand(address: Address): Term<Address> {
  return get(address);
}

export function debugExpand(address: Address) {
  const me: Term<Address> = expand(address);

  const ret = {};
  for (const [k, v] of me.entries()) {
    if (typeof v === 'string' && v.length === 64 && get(v) != null) {
      ret[k] = debugExpand(v);
    } else {
      ret[k] = v;
    }
  }

  return ret;
}
