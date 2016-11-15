// @flow
/* eslint-disable no-plusplus */

import { List as ExList } from 'extendable-immutable';
import { List } from 'immutable';
import { Block, Inline, Text } from 'slate';

import { expand, set as setAddr } from '../Address';
import { registerName } from '../name-registry';
import { Lisp, Ty } from '../forms/bootstrap';
import { tableRow, tableHeader, tableCell, mkStructuralKey, mkAddrKey, mkText }
  from '../slateHelpers';
import {
  hashable,
  fixedType,
  noAddressableChildren,
  fixedRepresentation,
  dispatchEvents,
  irreducible,
  noMeta,
  mapSubtermsIsMap,
} from '../decorators';
import { typecheck, TypecheckFailure } from '../unify';

import type { Address } from '../Address';
import type {
  ChildUpdate,
  Backspace,
  EditorEvent,
} from '../events';
import type { SlatePath, SlateVal } from '../slateHelpers';
import type {
  Term,
  Unif,
  UnificationProblem,
  UnificationResult,
  ComputationProperties,
  ValueProperties,
} from '../types';

let workspaceNameAddr;

export class RowsDontMatch extends TypecheckFailure {}
RowsDontMatch.value = new RowsDontMatch;

export class WorkspaceRelation {
  stitch(
    tm: Workspace<Address>,
    ty: WorkspaceTy<Address>
  ): [Workspace<Unif>, WorkspaceTy<Unif>] {
    // ew ew ew this is gross
    tm = tm || new Workspace();
    ty = ty || new WorkspaceTy();

    // for now, for simplicity, say that each row must match up directly
    if (tm.size === ty.size) {
      let accumTm = tm;
      let accumTy = ty;

      // stitch together children
      // and put them back into the original term / type
      tm.zip(ty)
        .forEach(([tmAddr: Address, tyAddr: Address], i) => {
          const tm_: Term<Address> = expand(tmAddr);
          const ty_: Term<Address> = expand(tyAddr);
          const checkResult = typecheck(tm_, ty_, tm_.proposeTypingRelation());

          if (checkResult instanceof TypecheckFailure) {
            // TODO distinguish between the two ends
            // this is actually really cool - this is where non-local mismatches
            // arise
            accumTm = accumTm.set(i, checkResult);
            accumTy = accumTy.set(i, checkResult);
          } else {
            const [tmChild, tyChild] = checkResult;
            accumTm = accumTm.set(i, setAddr(tmChild));
            accumTy = accumTy.set(i, setAddr(tyChild));
          }
        });

      return [accumTm, accumTy];
    } else {
      return RowsDontMatch.value;
    }
  }

  accepts(tm, ty) {
    return (
      (tm == null || tm instanceof Workspace) &&
      (ty == null || ty instanceof WorkspaceTy)
    );
  }
}

export class WorkspaceTyRelation {
  stitch(
    tm: WorkspaceTy<Address>,
    ty: Ty<Address> // eslint-disable-line no-unused-vars
  ): [WorkspaceTy<*>, Ty<*>] {
    // TODO there should be a decorator for this
    return [tm, ty];
  }

  accepts(tm, ty) {
    return tm instanceof WorkspaceTy && ty instanceof Ty;
  }
}

// eslint-disable-next-line no-unused-vars
export class WorkspaceTy<A> extends ExList {
  // TODO: check that each row is a type
  /*
  constructor(rows) {
    super(rows);
  }
  */

  proposeTypingRelation() {
    return new WorkspaceTyRelation;
  }

  // eslint-disable-next-line class-methods-use-this
  pullImpl() {
    return new Workspace([]);
  }

  slate(path: SlatePath): SlateVal {
    // TODO render children
    return new Inline({
      type: 'span',
      nodes: List([mkText('Workspace', mkStructuralKey(path, 0))]),
      key: mkAddrKey(this.hash, path),
    });
  }

  handleBackspace(evt: Backspace): ChildUpdate {
    return this.mkUpdate(new Lisp([workspaceNameAddr]), evt);
  }

  acceptChildUpdate(evt) {
    throw new Error('TODO: WorkspaceTy.acceptChildUpdate');
  }
}

WorkspaceTy.unifyChildren = function (
  exTyL: WorkspaceTy<Address>,
  exTyR: WorkspaceTy<Address>
): ?WorkspaceTy<Unif> {
  const result = exTyL.mergeWith(
    (l, r) => ({ l, r }),
    exTyR
  );
  console.log("WorkspaceTy here", {exTyL, exTyR, result});
  return result;
};


// eslint-disable-next-line no-unused-vars
workspaceNameAddr = registerName('workspace', (term, ty) => {
  throw new Error('TODO handleTypingAtEnd ) workspace');
});
hashable(WorkspaceTy);
// singletonType(WorkspaceTy);
fixedType(Ty.value)(WorkspaceTy); // Ty : Ty
mapSubtermsIsMap(WorkspaceTy);
fixedRepresentation(WorkspaceTy);
dispatchEvents(WorkspaceTy);
irreducible(WorkspaceTy);
noMeta(WorkspaceTy);
// trivialUnification(WorkspaceTy);

// eslint-disable-next-line no-unused-vars
export class Workspace<A> extends ExList {
  acceptChildUpdate(evt: ChildUpdate) {
    const { from, to } = evt;
    const newVal = this.mapSubterms(child => (child === from ? to : child));
    return this.mkUpdate(newVal, evt);
  }

  proposeTypingRelation() {
    return new WorkspaceRelation;
  }

  // eslint-disable-next-line class-methods-use-this
  handleTypingAtEnd(evt: EditorEvent) {
    // TODO throw instead?
    return evt;
  }

  // eslint-disable-next-line class-methods-use-this
  handleBackspace(evt: EditorEvent) {
    // TODO throw instead?
    return evt;
  }

  pushType(expectedTy: Term<Unif>): UnificationResult {
    if (expectedTy === WorkspaceTy) {
      // TODO a problem for each child!
      return { type: 'OuterSuccess', value: this };
    } else {
      // TODO should this be done by some higher machinery?
      const problem: UnificationProblem = { l: this, r: expectedTy };
      return { type: 'Failure', value: problem };
    }
  }

  pullType(): WorkspaceTy<Unif> {
    console.log(this)
    return new WorkspaceTy(
      this.mapSubterms(childAddr => {
        console.log(expand(childAddr))
        console.log(expand(childAddr).pullType())
        return expand(childAddr).pullType()
      })
    );
  }

  slate(path: SlatePath): SlateVal {
    let i = 0;
    const rows = this.mapSubterms((childAddr, childNum) => {
      const child: Term<Address> = expand(childAddr);
      const path_ = path.concat(childNum);
      // $FlowFixMe: I assert this is a valid cast -- more specific to less
      const modalityChild: ComputationProperties | ValueProperties = child;
      const evaluatedText: Text = modalityChild.modality === 'computation'
        ? expand(modalityChild.normalize())
            .slate(path_.concat(1))
        // HACK: + 100000 is an attempt to make ids stable here. If this
        // term is normalizable in one render and not in the next, then i
        // is incremented in only one of them, causing ids to change,
        // cascading across everything after. Mutability is not great.
        // TODO this is not a good long term solution
        : mkText('(not normalizable)', mkStructuralKey(path, i + 100000));

      // show the body, hash, and evaluated
      return tableRow(List([
        tableCell(
          List([child.slate(path_.concat(0))]),
          mkStructuralKey(path, i++)
        ),
        tableCell(
          List([mkText(childAddr.slice(0, 6), mkStructuralKey(path, i++))]),
          mkStructuralKey(path, i++), false
        ),
        tableCell(
          List([evaluatedText]),
          mkStructuralKey(path, i++),
          false
        ),
      ]), mkStructuralKey(path, i++));
    });

    const header = tableRow(List([
      tableHeader(
        List([mkText('definition', mkStructuralKey(path, i++))]),
        mkStructuralKey(path, i++)
      ),
      tableHeader(
        List([mkText('address', mkStructuralKey(path, i++))]),
        mkStructuralKey(path, i++)
      ),
      tableHeader(
        List([mkText('normalized', mkStructuralKey(path, i++))]),
        mkStructuralKey(path, i++)
      ),
    ]), mkStructuralKey(path, i++));

    rows.unshift(header);

    return new Block({
      type: 'table',
      nodes: List(rows),
      key: mkAddrKey(this.hash, path),
    });
  }
}

Workspace.unifyChildren = function (
  exL: Workspace<Address>,
  exR: Workspace<Address>
): Workspace<Unif> {
  console.log("Workspace here", {exL, exR});
  // join the two sets of workspace, can't error?
  // XXX this contradicts the type unification
  return exL.merge(exR);
};

hashable(Workspace);
dispatchEvents(Workspace);
irreducible(Workspace);
noMeta(Workspace);
mapSubtermsIsMap(Workspace);

// XXX
// fixedImpl(Workspace.value)(WorkspaceTy);
