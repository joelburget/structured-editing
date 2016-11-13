import { Character, Block, Text, Data } from 'slate';

import type { Inline } from 'slate';

export type SlateVal = Block | Inline | Text;
type SlatePath = Array<number>;

// We separate everything in keys with '_', since slate only supports \w+ keys.
export function mkAddrKey(hash: Address, path: SlatePath): string {
  return `addr_${path.join('_')}_${hash}`;
}

export function mkStructuralKey(path: SlatePath, uniq: number): string {
  return `structural_${path.join('_')}_${uniq}`;
}

export function mkText(text, key): SlateVal {
  const characters = Character.createListFromText(text, []);
  return Text.create({ key, characters });
}

export function tableHeader(nodes: List<SlateVal>, key: string): SlateVal {
  return new Block({ type: 'tableHeader', nodes, key });
}

export function tableRow(nodes: List<SlateVal>, key: string): SlateVal {
  return new Block({ type: 'tableRow', nodes, key });
}

export function tableCell(
  nodes: List<SlateVal>,
  key: string,
  contentEditable = true
): SlateVal {
  return new Block({
    type: 'tableCell',
    nodes,
    key,
    data: Data.create({ contentEditable }),
  });
}
