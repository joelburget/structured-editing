// @flow
/* eslint-disable no-use-before-define */

import type React from 'react';
import type { Record, List } from 'immutable';

import type { Address } from './Address';
import type {
  ChildUpdate,
  EditorEvent,
} from './events';
import type { SlatePath, SlateVal } from './slateHelpers';

export interface Traversable<A> {
  map<B>(f: (a: A) => B): Traversable<B>;
}

export type ComputationProperties = {
  modality: 'computation';
  normalize(): Address;
};

export type ValueProperties = {
  modality: 'value';
};

export type Unifiable = {
  // TODO this is actually static
  unifyChildren: (l: Term<Address>, r: Term<Address>) => ?Term<Unif>;
  proposeTypingRelation: () => TypingRelation;
};

// Every form must be convertible to lisp form by opening.
type LispConversion
  = { open: () => Term<Address> }
  // Except for Lisp, which is convertible to another term by closing
  | { close: () => Term<Address> };

// This is what a form looks like (plus LispConversion).
type FormPrecursor = {
  // handles an event and emits one for its parent to handle. typically either:
  // * don't handle the event -- just re-emit for parent
  // * handle and emit a `ChildUpdate`
  handle(evt: EditorEvent): EditorEvent;
  slate(path: SlatePath): SlateVal;
  acceptChildUpdate: (evt: ChildUpdate) => EditorEvent;
  meta(): Array<Meta>;
  hash: Address;
};

export type Form = FormPrecursor & LispConversion;

// phantom A:
// eslint-disable-next-line no-unused-vars
export type Term<A>
  = Unifiable
  & Form
  // Explicitly calling out the traversability that comes from the Record or
  // List
  & Traversable<A>
  // every term is either a value or computation
  & (ComputationProperties | ValueProperties)
  // Must subclass either immutable record or list
  & (Record<A> | List<A>);

export type Unif = Address | UnificationProblem;

export type UnificationProblem = {
  l: Address;
  r: Address;
};

export type UnificationOuterSuccess = {
  type: 'OuterSuccess';
  value: ?Term<Unif>;
};

export type UnificationOuterFailure = {
  type: 'Failure';
  value: UnificationProblem;
};

export type UnificationResult = UnificationOuterSuccess | UnificationOuterFailure;

export type TypecheckingOuterSuccess = {
  type: 'OuterSuccess';
  values: [?Term<Unif>, ?Term<Unif>];
};

export type TypecheckingOuterFailure = {
  type: 'Failure';
  values: [?Term<Unif> | UnificationProblem, ?Term<Unif> | UnificationProblem];
};

export type TypecheckResult = TypecheckingOuterSuccess | TypecheckingOuterFailure;

// TODO where to put this signature?
export type UnifyChildren = (l: Term<Address>, r: Term<Address>) => ?Term<Unif>;

export type MetaChoice = {
  choice: string;
  event: EditorEvent;
};

export type Meta = {
  // level = 'warning' | 'error';
  address: Address;
  message: React.Element<*>;
  choices: Array<MetaChoice>;
};

// Essentially the same as immutable's concept of path (theirs is a little
// wider)
export type Path = Array<string | number>;

export type ApplicationLocation = {
  at: Path;
  in: Address;
};

export type TypingRelation = {
  unifyTypeChildren: (tm: Term<Address>, ty: Term<Address>) => ?Term<Unif>;
};
