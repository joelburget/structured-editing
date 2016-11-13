import Uninterpreted from './forms/Uninterpreted';
import { set as setAddr } from './Address';

export function mkNameAddr(str) {
  return setAddr(new Uninterpreted({ str }));
}

type RegisteredName = {
  addr: Address;
  handler: Function;
}

// TODO:
// - allow registrations from outside this module
// - I'd love to get rid of all 'registry' concepts. This one is not really
//   baked in -- more of a cache.
export const names: { [key: string]: RegisteredName } = {};

export function registerName(name: string, handler: Function): Address {
  const addr = mkNameAddr(name);
  names[name] = { addr, handler };
  return addr;
}
