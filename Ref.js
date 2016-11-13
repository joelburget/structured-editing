// @flow

import { get as getAddr, set as setAddr } from './Address';

import type { Address } from './Address';

export type Ref = string;

const store = {};

export function get(ref: Ref) {
  return store[ref];
}

export function set(ref: Ref, addr: Address): void {
  store[ref] = addr;
}

export function getHashable(ref: Ref) {
  return getAddr(get(ref));
}

export function setHashable(ref: Ref, hashable: Object): void {
  set(ref, setAddr(hashable));
}
