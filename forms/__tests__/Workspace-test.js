/* eslint-env jest */

import { typecheck, unify, BadRelation } from '../../unify';
import { Ty } from '../bootstrap';
import { Int, IntTy } from '../Int';
import { set as setAddr } from '../../Address';
import { addLispAddr, oneAddr, twoAddr, onePlusTwoAddr }
  from '../../examples/Term';
import { Workspace, WorkspaceTy, WorkspaceRelation, RowsDontMatch } from '../Workspace';

describe('workspace row types', () => {
  test('Workspace <:~ WorkspaceTy', () => {
    const eTy = new WorkspaceTy([]);
    const [tm, ty] = typecheck(null, eTy, new WorkspaceRelation);

    expect(tm.hash).toBe(setAddr(new Workspace([])));
    expect(ty).toBe(eTy);
  });

  test('Workspace [1] :~ WorkspaceTy [Int]', () => {
    const e = new Workspace([oneAddr]);
    const eTy = new WorkspaceTy([IntTy.addr]);
    const [tm, ty] = typecheck(e, eTy, e.proposeTypingRelation());

    expect(tm.constructor).toBe(Workspace);
    expect(tm.get(0)).toBe(oneAddr);
    expect(ty.constructor).toBe(WorkspaceTy);
    expect(ty.get(0)).toBe(IntTy.addr);
  });

  test('Workspace [Ty] :/~ WorkspaceTy [Int]', () => {
    const e = new Workspace([Ty.addr]);
    const eTy = new WorkspaceTy([IntTy.addr]);
    const [tm, ty] = typecheck(e, eTy, e.proposeTypingRelation());

    expect(tm.constructor).toBe(Workspace);
    expect(tm.get(0)).toBeInstanceOf(BadRelation);
    expect(ty.constructor).toBe(WorkspaceTy);
    expect(ty.get(0)).toBeInstanceOf(BadRelation);
  });
});
