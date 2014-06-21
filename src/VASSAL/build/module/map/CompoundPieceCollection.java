/*
 * $Id$
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

import java.util.ArrayList;
import java.util.Arrays;

import VASSAL.counters.Deck;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;

/**
 * Base class for PieceCollection implementation that organize
 * pieces into distinct layers.  The layers are drawn in order of their index, i.e.
 * layer 0 is on the bottom.
 */
public abstract class CompoundPieceCollection implements PieceCollection {
  protected SimplePieceCollection[] layers;
  protected int bottomLayer = 0;
  protected boolean[] enabled;

  protected CompoundPieceCollection(int layerCount) {
    initLayers(layerCount);
  }

  protected void initLayers(int layerCount) {
    layers = new SimplePieceCollection[layerCount];
    enabled = new boolean[layerCount];
    for (int i=0;i<layers.length;++i) {
      layers[i] = new SimplePieceCollection();
      enabled[i] = true;
    }
  }

  public int getLayerForPiece(GamePiece p) {
    return 0;
  }

  public String getLayerNameForPiece(GamePiece p) {
    return "";
  }

  public int getLayerForName(String layerName) {
    return -1;
  }

  protected PieceCollection getCollectionForPiece(GamePiece p) {
    return layers[getLayerForPiece(p)];
  }

  public void add(GamePiece p) {
    getCollectionForPiece(p).add(p);
  }

  public void clear() {
    for (int i=0;i<layers.length;++i) {
      layers[i].clear();
    }
  }

  /*
   * Return pieces in layer order from the bottom up. Take into account
   * layer rotation and enabled state.
   */
  public GamePiece[] getPieces() {
    return getPieces(false);
  }

  protected GamePiece[] getPieces(boolean includeDisabled) {
    ArrayList<GamePiece> l = new ArrayList<GamePiece>();
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
    return l.toArray(new GamePiece[l.size()]);
  }

  public GamePiece[] getAllPieces() {
    return getPieces(true);
  }

  public int indexOf(GamePiece p) {
    int layer = getLayerForPiece(p);
    int index = layers[layer].indexOf(p);
    if (index >= 0) {
      for (int i=0;i<layer-1;++i) {
        index += layers[i].getPieces().length;
      }
    }
    return index;
  }

  public void remove(GamePiece p) {
    getCollectionForPiece(p).remove(p);
  }

  public void moveToBack(GamePiece p) {
    getCollectionForPiece(p).moveToBack(p);
  }

  public void moveToFront(GamePiece p) {
    getCollectionForPiece(p).moveToFront(p);
  }

  public boolean canMerge(GamePiece p1, GamePiece p2) {
    boolean canMerge = false;
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

  protected boolean canStacksMerge(Stack s1, Stack s2) {
    return canPiecesMerge(s1.topPiece(), s2.topPiece());
  }

  protected boolean canStackAndPieceMerge(Stack s, GamePiece p) {
    boolean canMerge = false;
    GamePiece top = s.topPiece();
    if (top != null) {
      canMerge = canPiecesMerge(top, p);
    }
    return canMerge;
  }

  protected boolean canPiecesMerge(GamePiece p1, GamePiece p2) {
    boolean canMerge = false;
    if (p1 != null
        && p2 != null) {
      canMerge = !Boolean.TRUE.equals(p1.getProperty(Properties.NO_STACK))
          && !Boolean.TRUE.equals(p2.getProperty(Properties.NO_STACK))
          && !Boolean.TRUE.equals(p1.getProperty(Properties.INVISIBLE_TO_ME))
          && !Boolean.TRUE.equals(p2.getProperty(Properties.INVISIBLE_TO_ME));
    }
    return canMerge;
  }

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

  public int getBottomLayer() {
    return bottomLayer;
  }

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
      setBottomLayer(bottomLayer-1);
    }
    else {
      setBottomLayer(bottomLayer + 1);
    }
  }

  /*
   * Enable/Disable layers
   */
  public void setLayerEnabled(int layer, boolean b) {
    if (layer >= 0 && layer < layers.length) {
      enabled[layer] = b;
    }
  }

  public void toggleLayerEnabled(int layer) {
    if (layer >= 0 && layer < layers.length) {
      enabled[layer] = !enabled[layer];
    }
  }

  public void setLayerEnabled(String layer, boolean b) {
    setLayerEnabled(getLayerForName(layer), b);
  }

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
