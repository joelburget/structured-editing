// @flow
/* eslint-env jest */

import { set as setAddr, expand, debugExpand } from '../../Address';
import { Application } from '../Application';
import { typecheck, unify, BadRelation } from '../../unify';
import { Rec, RecTy, RecRelation, FieldAccess } from '../Rec';
import { Int, IntTy, IntRelation } from '../Int';
import { addLispAddr, oneAddr, twoAddr, onePlusTwoAddr }
  from '../../examples/Term';
import Uninterpreted from '../Uninterpreted';
import expectIs from '../../expectIs';

const rec: Address = setAddr(new Rec({ a: oneAddr }));
const recTy = new RecTy({ a: IntTy.addr });
const name: Address = setAddr(new Uninterpreted({ str: 'a' }));
const computation: FieldAccess<Address> = new FieldAccess({ name });
const computationAddr: Address = setAddr(computation);
const app = new Application([computationAddr, rec]);
const appAddr = setAddr(app);

describe('unification', () => {
  test('{ a: 1 } ~ { b: 2 }', () => {
    const recA = new Rec({ a: oneAddr });
    const recB = new Rec({ b: twoAddr });
    const { value } = unify(setAddr(recA), setAddr(recB));

    expectIs(value, new Rec({
      a: oneAddr,
      b: twoAddr,
    }));
  });
});

describe('typechecking', () => {
  test('{ a: 1 } :~ { a: Int }', () => {
    const rec_ = expand(rec);
    const [tm, ty] = typecheck(rec_, recTy, RecRelation.value);

    expect(tm.constructor).toBe(Rec);
    expect(tm.get('a')).toBe(oneAddr);
    expect(ty.constructor).toBe(RecTy);
    expect(ty.get('a')).toBe(IntTy.addr);
  });

  it('{ a: 1 }.a :~ Int', () => {
    const [tm, ty] = typecheck(app, IntTy.value, IntRelation.value);

    expect(tm.hash).toBe(oneAddr);
    expect(ty).toBe(IntTy.value);
  });
});

describe('evaluation', () => {
  test('{ a: 1 }.a -> 1', () => {
    const normalized = app.annihilate();
    expect(normalized).toBe(oneAddr);
  });
});

// TODO: when we have variables we should return any solving / instantiation we
// had to do while unifying

// TODO: something less trivial, like showing `2 + _ ~ _ + 2` in Peano
// arithmetic. To do this the way I want I think will require programmer help.

describe('unification / evaluation', () => {
  it('{ a: 1 }.a ~ 1', () => {
    const { value } = unify(appAddr, oneAddr);

    expectIs(value, new Int({ i: 1 }));
  });

  // { a: { a: 1 }.a }
  const rec_: Address = setAddr(new Rec({ a: appAddr }));
  // (...).a
  const computationAddr_ = setAddr(new FieldAccess({ name, rec: rec_ }));
  const appAddr_ = setAddr(
    new Application([computationAddr_, rec_])
  );

  // both sides should just reduce to `1`
  test('{ a: { a: 1 }.a }.a ~ { a: 1 }.a', () => {
    const { value } = unify(appAddr_, appAddr);

    expectIs(value, new Int({ i: 1 }));
  });

  test('{ a: { a: 1 }.a }.a ~ 1', () => {
    const { value } = unify(appAddr_, oneAddr);

    expectIs(value, new Int({ i: 1 }));
  });
});
