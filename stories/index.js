// @flow
/* eslint-disable import/no-extraneous-dependencies, import/no-unresolved */
import React from 'react';
import { storiesOf, action } from '@kadira/storybook';
import StructuredEditor from '../components/StructuredEditor';
import { names } from '../name-registry';
import { Workspace } from '../forms/Workspace';
import { Rec, RecTy } from '../forms/Rec';
import { applyChange, traverse } from '../events';
import { set as setAddr, debugExpand } from '../Address';
import { BoolTy } from '../forms/Bool';
import { IntTy } from '../forms/Int';
import { NominalVariant, NominalVariantTy } from '../forms/Variant';
import Arrow from '../forms/Arrow';
import Uninterpreted from '../forms/Uninterpreted';

import {
  oneAddr,
  twoAddr,
  onePlusTwoAddr,
  emptyLispAddr,
  addLispAddr,
  conflictExampleAddr,
  annotationExampleAddr,
  workspaceExampleAddr,
  doubleConflictAddr,
} from '../examples/Term';

// $FlowFixMe: "required module not found"
require('expose?React!react');
// $FlowFixMe: "required module not found"
require('expose?Perf!react-addons-perf');
// $FlowFixMe: "required module not found"
require('expose?Immutable!immutable');

const onChange = action('onChange');


storiesOf('Structured Display', module)
  .add('1 + 2', () => (
    <StructuredEditor location={onePlusTwoAddr} onChange={onChange} />
  ))
  .add('empty lisp', () => (
    <StructuredEditor location={emptyLispAddr} onChange={onChange} />
  ))
  .add('add lisp', () => (
    <StructuredEditor location={addLispAddr} onChange={onChange} />
  ))
  .add('conflict', () => (
    <StructuredEditor location={conflictExampleAddr} onChange={onChange} />
  ))
  .add('annotation', () => (
    <StructuredEditor location={annotationExampleAddr} onChange={onChange} />
  ))
  .add('workspace', () => (
    <StructuredEditor location={workspaceExampleAddr} onChange={onChange} />
  ))
  .add('double conflict', () => (
    <StructuredEditor location={doubleConflictAddr} onChange={onChange} />
  ))
  .add('double conflict resolution', () => {
    // 1 + (true || 2)
    const step0 = doubleConflictAddr;
    console.log(debugExpand(step0));

    // =>
    // 1 + (|| true 2
    // poke the || conflict
    const { to: step1 } = applyChange(
      { type: 'ConflictToLisp' },
      { at: ['r'], in: step0 }
    );
    console.log(debugExpand(step1));

    // =>
    // 1 + (add true 2
    const from = traverse(step1, ['r', 0]);
    const x = names.add.addr;
    debugger;
    const { to: step2 } = applyChange(
      { type: 'Substitute',
        from,
        to: x,
      },
      { at: ['r', 0], in: step1 }
    );
    console.log(debugExpand(step2));

    // =>
    // 1 + (true + 2)
    const { to: step3 } = applyChange(
      { type: 'Close' },
      { at: ['r'], in: step2 }
    );
    console.log(debugExpand(step3));

    // =>
    // 1 + (2 + 2)
    // (kind of skipping a step, whatever)
    const { to: step4 } = applyChange(
      { type: 'Substitute' },
      { at: ['r', 'l'], in: step3 }
    );
    console.log(debugExpand(step4));

    const location = setAddr(new Workspace({
      children: [step0, step1, step2, step3, step4],
    }));

    return <StructuredEditor location={location} onChange={onChange} />;
  })
  .add('records', () => {
    const recAddr = setAddr(new Rec({
      a: oneAddr,
      b: twoAddr,
    }));

    return (
      <StructuredEditor location={recAddr} onChange={onChange} />
    );
  })
  .add('record types', () => {
    // This is the type of the last thing
    const recTyAddr = setAddr(new RecTy({
      a: IntTy.addr,
      b: IntTy.addr,
    }));

    return (
      <StructuredEditor location={recTyAddr} onChange={onChange} />
    );
  })
  .add('variants', () => {
    const vAddr = setAddr(new NominalVariant({ tag: 'a' }));

    return (
      <StructuredEditor location={vAddr} onChange={onChange} />
    );
  })
  .add('variant types', () => {
    const aAddr = setAddr(new Uninterpreted({ str: 'a' }))
    const bAddr = setAddr(new Uninterpreted({ str: 'b' }))
    const cAddr = setAddr(new Uninterpreted({ str: 'c' }))
    const vTyAddr = setAddr(new NominalVariantTy([aAddr, bAddr, cAddr]));

    return (
      <StructuredEditor location={vTyAddr} onChange={onChange} />
    );
  })
  .add('arrows', () => {
    const arrAddr = setAddr(new Arrow({
      domain: IntTy.addr,
      codomain: BoolTy.addr,
    }));

    return (
      <StructuredEditor location={arrAddr} onChange={onChange} />
    );
  })
  ;
