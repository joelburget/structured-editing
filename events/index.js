import { expand } from '../Address';

import type { Address } from '../Address';
import type { ApplicationLocation } from '../types';

export type TypingAtEnd = {
  type: 'TypingAtEnd';
  textValue: string;
  insertedText: string;
};

export type Backspace = {
  type: 'Backspace';
  textValue: string;
};

// TODO replace with 'BubbleChange'
export type ChildUpdate = {
  type: 'ChildUpdate';
  from: Address;
  to: Address;
  // eslint-disable-next-line no-use-before-define
  provenance: EditorEvent;
};

export type ResolveConflict = {
  type: 'ResolveConflict';
  tyChoice: Address;
};

export type QueryMeta = {
  type: 'QueryMeta';
  soFar: Array<Address>;
};

export type ConflictToLisp = {
  type: 'ConflictToLisp';
};

export type Close = { type: 'Close' };

export type Open = { type: 'Open' };

// Differs from ChildUpdate / BubbleChange because it's user-initiated
// TODO implement on bool / all
export type Substitute = {
  type: 'Substitute';
  from: Address;
  to: Address;
};

// TODO maybe makes sense to update the name from EditorEvent since some of
// these don't originate from the editor anymore.
// TODO is there a distinction between a regular event and the concept of a
// change?
export type EditorEvent
  = TypingAtEnd
  | Backspace
  | ChildUpdate
  | ResolveConflict
  | QueryMeta
  | ConflictToLisp
  | FillHole
  | Substitute
  | Open
  ;

export function applyChange(
  evt: EditorEvent,
  { at, in: start }: ApplicationLocation
): Address {
  const me: Term<Address> = expand(start);

  if (at.length === 0) {
    return me.handle(evt);
  } else {
    const [head, ...tail] = at;
    const from: Address = me.get(head);
    const provenance: ChildUpdate = applyChange(evt, { at: tail, in: from });
    const to = provenance.to;
    return me.handle({ type: 'ChildUpdate', from, to, provenance });
  }
}

export function traverse(location: Address, path: Path): Address {
  if (path.length === 0) {
    return location;
  } else {
    const [head, ...tail] = path;
    const newLocation = expand(location).get(head);
    return traverse(newLocation, tail);
  }
}

// export function evtFindIx(evt: ChildUpdate, name: string): number {
//   let i = 0
//   while (true) {
//     if (evt.type === name) return i;

//     i++;
//     evt = evt.provenance;
//   }
// }

// export function evtGetIx(evt: ChildUpdate, ix: number): EditorEvent {
//   for (let i = 0; i < ix; i++) {
//     if (i === 0) return evt;

//     evt = evt.provenance;
//   }
// }

// export function evtGetUpdateTo(evt: ChildUpdate, name: string): ChildUpdate {
//   const evtIx = evtFindIx(evt, name);
//   return evtGetIx(evt, evtIx - 1);
// }
