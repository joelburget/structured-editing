// @flow
import React from 'react';
import { Document, State, Selection, Editor, Block } from 'slate';
import { List } from 'immutable';

import type { State as EditorState } from 'slate';

import { expand } from '../Address';
import schema from './SlateNodes';
import bubble from '../bubble';

import type { Address } from '../Address';
import type { EditorEvent, Term } from '../events';
import type { Meta } from '../types';

type StructuredEditorProps = {
  location: Address;
};

type StructuredEditorState = {
  state: Term;
  metaAddresses: Array<Address>;
  root: Address;
};

const styles = {
  meta: {
    fontFamily: 'monospace',
  },
  button: {
    backgroundColor: 'white',
    border: '2px solid #ddd',
    padding: 5,
    margin: 5,
  },
};

function flatten<A>(as: Array<Array<A>>): Array<A> {
  return [].concat(...as);
}

function slateFromAddress(root: Address, document: Document): Document {
  const node: Term = expand(root);
  const path = [];
  const nodes = List([node.slate(path)]);

  // we absolutely must avoid calling `Document.create` on a substantial
  // document -- it's super slow because it traverses the entire tree in an
  // expensive way.
  return document.set('nodes', List([new Block({ type: 'div', nodes })]));
}

function initState(root: Address): State {
  const document = slateFromAddress(root, Document.create());
  return State.create({ document });
}

function mkStructuredEditorState(root: Address, state: State): State {
  const document = slateFromAddress(root, state.document);

  const hasAnchorFocus =
    document.getDescendant(state.anchorKey) != null &&
    document.getDescendant(state.focusKey) != null;

  const stateWithSelection = hasAnchorFocus
    ? state
    // TODO find an appropriate new selection
    : state.set('selection', new Selection());

  return stateWithSelection.set('document', document);
}

export default class StructuredEditor
  extends React.Component<{}, StructuredEditorProps, StructuredEditorState> {

  static defaultProps = {};

  constructor(props: StructuredEditorProps) {
    super(props);
    const { location } = props;

    this.state = {
      state: initState(location),
      root: location,
      metaAddresses: [],
    };

    this.onChange = state => this._onChange(state);
  }

  state: StructuredEditorState;

  onChange: (state: Term) => void;
  _onSelectionChange: (sel: Selection, state: EditorState) => Array<Address>;
  _onDocumentChange: (document: Document, state: EditorState) =>
    [EditorState, Address];

  // We dispatch to _onDocumentChange and _onSelectionChange ourselves
  // instead of letting slate do it, because slate unfortunately can't pass
  // the updated state from the first (_onDocumentChange in this case) to the
  // second (_onSelectionChange), leading to overwritten / missing updates.
  _onChange(state: EditorState): void {
    const newState = {};
    const { document, selection } = state;
    if (state.document !== this.state.state.document) {
      const [cState, cRoot] = this._onDocumentChange(document, state);
      newState.state = cState;
      newState.root = cRoot;
    }

    if (selection !== this.state.state.selection) {
      newState.metaAddresses = this._onSelectionChange(state.selection, state);
    }

    this.setState(newState);
  }

  dispatchTo(address: Address, event: EditorEvent): void {
    const { state } = this.state;
    const { document } = state;
    const node = document.findDescendant(
      desc => desc.key != null && desc.key.includes(address)
    );
    const { to: root } = bubble(event, document, node.key);
    const newState = mkStructuredEditorState(
      root,
      state.set('document', document)
    );
    this.setState({ state: newState, root });
  }

  // eslint-disable-next-line class-methods-use-this
  _onSelectionChange(selection: Selection, state: EditorState):
    Array<Address> {
    if (selection.anchorKey == null) return [];

    const startKey = selection.isExpanded
      ? state.document.getCommonAncestor(selection.startKey, selection.endKey).key
      : selection.startKey;

    // See if this selection is within a conflict -- if so, show the resolution
    // interface.
    const evt = {
      type: 'QueryMeta',
      soFar: [],
    };

    const topQ = bubble(evt, state.document, startKey);
    return topQ.soFar;
  }

  // In general, we need to wait until *after* text is inserted to handle it.
  // In `onKeyDown` we don't have a reliable way of knowing whether the
  // keypress will produce text or not.
  //
  // We wait until the document has changed, then look back at the last item in
  // history. If it's text insertion, then we check what was inserted.
  // eslint-disable-next-line class-methods-use-this
  _onDocumentChange(document: Document, state: EditorState):
    [EditorState, Address] {
    const { history, selection } = state;
    const { anchorKey } = selection;
    const undos = history.undos;

    if (state.hasUndos) {
      // I'm seeing the last operation always followed by a 'set_selection'.
      // Check from the end until we find the right one.
      const lastUndoGroup = undos.get(0);
      let lastUndo = null;
      for (let i = lastUndoGroup.length - 1; i >= 0; i -= 1) {
        if (lastUndoGroup[i].type !== 'set_selection') {
          lastUndo = lastUndoGroup[i];
          break;
        }
      }

      if (lastUndo == null) {
        throw new Error('invariant violation');
      }

      const node = document.getParent(anchorKey);
      let evt: EditorEvent;
      if (lastUndo.type === 'remove_text') {
        evt = {
          type: 'Backspace',
          textValue: node.text,
        };
      } else if (lastUndo.type === 'insert_text') {
        evt = {
          type: 'TypingAtEnd',
          textValue: node.text,
          insertedText: lastUndo.text,
        };
      } else {
        // TODO learn how to handle this (not an insert or deleteBackward)
        throw new Error('StructuredEditor _onDocumentChange unimplemented');
      }
      const topEvt = bubble(evt, document, node.key);
      const root = topEvt.to;

      // TODO set selection here -- we should probably get selection
      // information from the update with the handling node (the text node #
      // and index in that text node), from which we can reconstruct the
      // selection info slate expects

      const newState = mkStructuredEditorState(
        root,
        state.set('document', document)
      );
      return [newState, root];
    } else {
      // If there's no history ignore. This means there was not a modifying
      // change, which probably means it was just a selection change.
      // TODO a selection change should never end up here -- throw?
      throw new Error(
        'TODO unexpected lack of history in StructuredEditor._onDocumentChange'
      );
    }
  }

  render() {
    const { state, metaAddresses } = this.state;

    const metas: Array<Meta> =
      flatten(metaAddresses.map(address => expand(address).meta()));

    const metaElems: Array<React.Element<*>> =
      metas.map(({ address, message, choices }) => (
        <div>
          {message}
          <div>
            {choices.map(({ choice, event }) => (
              <button
                onClick={() => this.dispatchTo(address, event)}
                style={styles.button}
              >
                {choice}
              </button>
            ))}
          </div>
        </div>
      ));
    const metaSection = metaElems.length > 0 && (
      <div style={styles.meta}>
        <h2>meta</h2>
        {metaElems}
      </div>
    );

    return (
      <div>
        <div style={{ fontFamily: 'monospace' }}>
          <Editor
            state={state}
            onChange={this.onChange}
            schema={schema}
            spellCheck={false}
          />
        </div>
        {metaSection}
      </div>
    );
  }
}
