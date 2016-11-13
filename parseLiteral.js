// @flow

type NumberLiteral = {
  type: 'number';
  value: number;
};

type StringLiteral = {
  type: 'string';
  value: string;
};

type RefLiteral = {
  type: 'ref';
  name: ?string;
  hash: ?string;
};

export type Literal = NumberLiteral | StringLiteral | RefLiteral;

export default function parseLiteral(str: string): ?Literal {
  if (str.trim() !== str) {
    return null;
  }

  // TODO this seems to trim first so it parses, eg, " 2 "
  const attemptedNum = +str;

  // first try number
  if (!Number.isNaN(attemptedNum)) {
    return { type: 'number', value: attemptedNum };
  }

  // next string (extremely limited right now -- starts and ends with quotes,
  // contains no others)
  if (str[0] === '"' &&
      str[str.length - 1] === '"' &&
      !str.substr(1, str.length - 2).includes('"')) {
    // TODO include quotes?
    return { type: 'string', value: str };
  }

  // last, try to parse a ref / addr
  // format:
  //   - ([\w]{3,})?(#[0-9a-fA-F]{2,64})?
  //   - must include either name or sha
  //
  // (names required to be at least three characters, hashes at least two)
  const refRegExp = /^([\w]{3,})?(#[0-9a-fA-F]{2,64})?$/;
  if (refRegExp.test(str)) {
    const [_, name, hash] = refRegExp.exec(str);

    // make sure we have either a name or hash
    return name !== undefined || hash !== undefined
      ? { type: 'ref', name, hash }
      : null;
  }

  return null;
}
