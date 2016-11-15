// @flow

import { List } from 'immutable';
import { Block, Inline } from 'slate';
import { tableRow, tableCell, mkStructuralKey, mkAddrKey, mkText }
  from './slateHelpers';
import { expand } from './Address';

import type { Form } from './types';
import type { SlatePath, SlateVal } from './slateHelpers';

export default function recordLikeRender(tm: Form, path: SlatePath): SlateVal {
  const rows = tm.subterms.map((childAddr, name) => {
    const child = expand(childAddr);
    const path_ = path.concat(name);
    let i = 0;

    return tableRow(List([
      tableCell(
        List([mkText(name, mkStructuralKey(path, i++))])
      ),
      tableCell(
        List([child.slate(path_.concat(1))])
      ),
    ]));
  });

  return new Block({
    type: 'table',
    nodes: rows,
    key: mkAddrKey(tm.hash, path),
  });
}
