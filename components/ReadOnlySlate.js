// @flow

import React from 'react';
import { List } from 'immutable';
import { Block, Inline, Editor, Document, State } from 'slate';

import { expand } from '../Address';

import type { Address } from '../Address';

export default function ReadOnlySlate({ address }: { address: Address }) {
  const slated = expand(address).slate([]);
  const style = slated instanceof Inline
    ? { display: 'inline-block', margin: '0 5px' }
    : null;
  const nodes = List([slated]);
  const document = Document
    .create()
    .set('nodes', List([new Block({ type: 'div', nodes })]));
  const state = State.create({ document });
  return <Editor state={state} style={style} readOnly />;
}
