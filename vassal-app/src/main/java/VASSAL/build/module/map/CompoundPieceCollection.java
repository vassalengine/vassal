/*
 *
 * Copyright (c) 2004 by Rodney Kinney
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.build.module.map;

import VASSAL.counters.Deck;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Objects;

/**
 * Base class for PieceCollection implementation that organizes {@link GamePiece}s -- a category which in this case
 * also includes Decks and Stacks, in addition to ordinary pieces -- into distinct visual layers. The layers are drawn
 * in order of their index, i.e. layer 0 is drawn first and thus appears "on the bottom", as higher layers are drawn
 * over top of it.
 * <br><br>
 * Maintains, usually on behalf of a Map, lists of all the pieces in each of several layers, along with a set of "enabled"
 * flags marking which layers are disabled/hidden and which are enabled/visible. When a piece is added to the overall collection,
 * its appropriate layer is determined and it is added to the list of pieces on that layer.
 */
public abstract class CompoundPieceCollection implements PieceCollection {
  protected SimplePieceCollection[] layers; // List of pieces in each layer
  protected int bottomLayer = 0;            // Current bottom layer (provides option to rotate layer depth)
  protected boolean[] enabled;              // Flags indicating which layers are presently enabled/visible

  protected CompoundPieceCollection(int layerCount) {
    initLayers(layerCount);
  }

  protected void initLayers(int layerCount) {
    layers = new SimplePieceCollection[layerCount];
    enabled = new boolean[layerCount];
    for (int i = 0; i < layers.length; ++i) {
      layers[i] = new SimplePieceCollection();
      enabled[i] = true;
    }
  }

  /**
   * Default implementation is "degenerate", having only a single layer -- when extending this class, this method
   * takes a piece and determines which of several layers it belongs in, returning an index.
   * @param p A game piece
   * @return Index for the visual layer the piece should be drawn with.
   */
  public int getLayerForPiece(GamePiece p) {
    return 0;
  }

  /**
   * Default implementation is "degenerate", having only a single layer -- when extending this class, this method
   * takes a piece and determines which of several layers it belongs in, returning the layer name.
   * @param p A game piece
   * @return the layer name it belongs in.
   */
  public String getLayerNameForPiece(GamePiece p) {
    return ""; //NON-NLS
  }

  /**
   * Default implementation is "degenerate", having only a single layer -- when extending this class, this method
   * takes layer name and returns the index for that layer, or -1 if the string does not name a valid layer.
   * @param layerName the name of a layer
   * @return the index for the layer
   */
  public int getLayerForName(String layerName) {
    return -1;
  }

  /**
   * Given a game piece, returns the simple piece collection for the layer that it belongs in
   * @param p A game piece
   * @return the piece collection for the layer that it belongs in
   */
  protected PieceCollection getCollectionForPiece(GamePiece p) {
    return layers[getLayerForPiece(p)];
  }

  /**
   * Adds a piece to the overall collection, by adding it to the simple collection for the layer it belongs in.
   * @param p Game piece to add
   */
  @Override
  public void add(GamePiece p) {
    getCollectionForPiece(p).add(p);
  }

  /**
   * Clears the whole collection.
   */
  @Override
  public void clear() {
    for (final SimplePieceCollection layer : layers) {
      layer.clear();
    }
  }

  /*
   * Return pieces in layer order from the bottom up. Take into account
   * layer rotation and enabled state.
   */
  @Override
  public GamePiece[] getPieces() {
    return getPieces(false);
  }

  /**
   * @param includeDisabled true if pieces in disabled layers should be included
   * @return A list of all pieces in this overall collection, or all that are in "enabled" layers, depending on the parameter
   */
  protected GamePiece[] getPieces(boolean includeDisabled) {
    final ArrayList<GamePiece> l = new ArrayList<>();
    int layer = bottomLayer;
    for (int i = 0; i < layers.length; ++i) {
      if (includeDisabled || (!includeDisabled && enabled[layer])) {
        l.addAll(Arrays.asList(layers[layer].getPieces()));
      }
      layer++;
      if (layer >= layers.length) {
        layer = 0;
      }
    }
    return l.toArray(new GamePiece[0]);
  }

  /**
   * @return a list of all pieces in any layer of this collection.
   */
  @Override
  public GamePiece[] getAllPieces() {
    return getPieces(true);
  }

  /**
   * @param p A game piece
   * @return A unique index for the game piece within the overall collection.
   */
  @Override
  public int indexOf(GamePiece p) {
    final int layer = getLayerForPiece(p);
    int index = layers[layer].indexOf(p);
    if (index >= 0) {
      for (int i = 0; i < layer - 1; ++i) {
        index += layers[i].getPieces().length;
      }
    }
    return index;
  }

  /**
   * @param p piece to remove from the collection
   */
  @Override
  public void remove(GamePiece p) {
    getCollectionForPiece(p).remove(p);
  }

  /**
   * @param p piece to move to the visual back of its layer
   */
  @Override
  public void moveToBack(GamePiece p) {
    getCollectionForPiece(p).moveToBack(p);
  }

  /**
   * @param p piece to move to visual front of its layer
   */
  @Override
  public void moveToFront(GamePiece p) {
    getCollectionForPiece(p).moveToFront(p);
  }

  /**
   * Used when moving a piece on top of another piece to determine whether they can be merged together
   * (e.g. can this Piece be added to that Stack, can this Stack be added to that Deck, can these two
   * Stacks be combined together into one). Default preconditions for merging into a Stack include not
   * being Invisible due to a {@link VASSAL.counters.Hideable} trait, and not having a "Does Not Stack"
   * trait ({@link VASSAL.counters.Immobilized}). When extending the class additional requirements can
   * be added (e.g. {@link LayeredPieceCollection} requires pieces to be in the same visual layer in
   * order to form a stack).
   * @param p1 one piece/stack/deck
   * @param p2 another piece/stack/deck
   * @return Whether the two pieces are legal to merge
   */
  @Override
  public boolean canMerge(GamePiece p1, GamePiece p2) {
    final boolean canMerge;
    if (p1 instanceof Deck
        || p2 instanceof Deck) {
      canMerge = true;
    }
    else if (p1 instanceof Stack) {
      if (p2 instanceof Stack) {
        canMerge = canStacksMerge((Stack) p1, (Stack) p2);
      }
      else {
        canMerge = canStackAndPieceMerge((Stack) p1, p2);
      }
    }
    else if (p2 instanceof Stack) {
      canMerge = canStackAndPieceMerge((Stack) p2, p1);
    }
    else {
      canMerge = canPiecesMerge(p1, p2);
    }
    return canMerge;
  }

  /**
   * Stacks can merge if their component pieces can merge
   * @param s1 stack
   * @param s2 another stack
   * @return can they make one stack?
   */
  protected boolean canStacksMerge(Stack s1, Stack s2) {
    return canPiecesMerge(s1.topPiece(), s2.topPiece()); //NOTE: topPiece() returns the top VISIBLE piece (not hidden by Invisible trait)
  }

  /**
   * A piece can be merged into a stack if it is merge-compatible with the first piece in the stack.
   * @param s stack
   * @param p piece
   * @return can they make one stack?
   */
  protected boolean canStackAndPieceMerge(Stack s, GamePiece p) {
    boolean canMerge = false;
    final GamePiece top = s.topPiece();  //NOTE: topPiece() returns the top VISIBLE piece (not hidden by Invisible trait)
    if (top != null) {
      canMerge = canPiecesMerge(top, p);
    }
    return canMerge;
  }

  /**
   * Two pieces can merge in the default case as long as neither is presently invisible and neither has a
   * Does Not Stack ({@link VASSAL.counters.Hideable}) trait.
   * @param p1 a piece
   * @param p2 another piece
   * @return WonderTwin powers activate?
   */
  protected boolean canPiecesMerge(GamePiece p1, GamePiece p2) {
    boolean canMerge = false;
    if (p1 != null
        && p2 != null) {
      canMerge = !Boolean.TRUE.equals(p1.getProperty(Properties.NO_STACK))
          && !Boolean.TRUE.equals(p2.getProperty(Properties.NO_STACK))
          && ((!Boolean.TRUE.equals(p1.getProperty(Properties.INVISIBLE_TO_ME)) && !Boolean.TRUE.equals(p2.getProperty(Properties.INVISIBLE_TO_ME))
              || Objects.equals(p1.getProperty(Properties.HIDDEN_BY), p2.getProperty(Properties.HIDDEN_BY))));
    }
    return canMerge;
  }

  /**
   * @return the number of visual layers in this collection
   */
  public int getLayerCount() {
    return layers.length;
  }

  /*
   * Set a new bottom layer. Take care of wrapping around ends of layer list.
   */
  public void setBottomLayer(int layer) {
    bottomLayer = layer;
    if (bottomLayer < 0) bottomLayer = getLayerCount() - 1;
    if (bottomLayer >= getLayerCount()) bottomLayer = 0;
  }

  /**
   * @return current bottom layer
   */
  public int getBottomLayer() {
    return bottomLayer;
  }

  /**
   * @return the current top layer
   */
  public int getTopLayer() {
    int layer = bottomLayer - 1;
    if (layer < 0) {
      layer = getLayerCount() - 1;
    }
    return layer;
  }

  /*
   * Rotate layers up or down, optionally skipping top layers not containing
   * counters.
   */
  public void rotate(boolean rotateUp, boolean skipNullLayers) {
    if (skipNullLayers) {
      for (int i = 0; i < layers.length; i++) {
        rotate(rotateUp);
        if (layers[getTopLayer()].getPieces().length > 0) {
          return;
        }
      }
    }
    else {
      rotate(rotateUp);
    }
  }

  /*
   * Rotate Layers up or down by 1 layer
   */
  public void rotate(boolean rotateUp) {
    if (rotateUp) {
      setBottomLayer(bottomLayer - 1);
    }
    else {
      setBottomLayer(bottomLayer + 1);
    }
  }

  /**
   * Enable/Disable layers
   * @param layer layer index
   * @param b true to enable the layer, false to disable
   */
  public void setLayerEnabled(int layer, boolean b) {
    if (layer >= 0 && layer < layers.length) {
      enabled[layer] = b;
    }
  }

  /**
   * Toggle for Enable/Disable of layers
   * @param layer layer index
   */
  public void toggleLayerEnabled(int layer) {
    if (layer >= 0 && layer < layers.length) {
      enabled[layer] = !enabled[layer];
    }
  }

  /**
   * Enable/Disable layers
   * @param layer layer name
   * @param b true to enable the layer, false to disable
   */
  public void setLayerEnabled(String layer, boolean b) {
    setLayerEnabled(getLayerForName(layer), b);
  }

  /**
   * Toggle for Enable/Disable of layers
   * @param layer layer name
   */
  public void toggleLayerEnabled(String layer) {
    toggleLayerEnabled(getLayerForName(layer));
  }

  /*
   * Reset Layers to original state
   */
  public void reset() {
    setBottomLayer(0);
    for (int i = 0; i < layers.length; i++) {
      enabled[i] = true;
    }
  }
}
