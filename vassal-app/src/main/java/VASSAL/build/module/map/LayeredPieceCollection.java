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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.JToolBar;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.SingleChildInstance;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.counters.Deck;
import VASSAL.counters.DeckVisitor;
import VASSAL.counters.DeckVisitorDispatcher;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;
import VASSAL.tools.TemporaryToolBar;

import static VASSAL.counters.Stack.LAYER_NOT_SET;

/**
 * The optional "Game Piece Layers" component of a Map which allows pieces on the map to be assigned to an
 * arbitrary number of visual layers according to a property setting. The (inauspiciously-named) Collection subclass
 * becomes a {@link CompoundPieceCollection} override for the Map's default PieceCollection, in order to provide the
 * additional functionality.
 * <br><br>
 * Unlike most "apparent properties of pieces", individual pieces know their layer only indirectly through providing
 * the string property name -- they do not e.g. have a convenient way to check which <i>relative</i> layer they are in.
 * Instead, the Map (through this class) maintains separate lists of the pieces in each visual layer, as well as a
 * set of flags allowing individual layers to be hidden from view. When the Map is asked to draw pieces in a region,
 * it only draws the ones for which the designated layer is currently visible.
 * <br><br>
 * Note that {@link Stack}s, since they often serve as drawing proxies for their member pieces, must also be aware of
 * their proper layer, and since "in theory" Stacks are only formed from stackable units in the same visual layer,
 * they take their layer cues from the first piece added to them.
 */
public class LayeredPieceCollection extends AbstractConfigurable {
  public static final String PROPERTY_NAME = "property"; //NON-NLS
  public static final String LAYER_ORDER = "layerOrder"; //NON-NLS
  protected Collection collection = new Collection(Resources.getString("Editor.LayeredPieceCollection.layer"), new String[0]); //NON-NLS // "layer"
  protected Map map;
  protected TemporaryToolBar tempToolBar;

  public LayeredPieceCollection() {
    this.setAttributeTranslatable(PROPERTY_NAME, false);
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
        Resources.getString("Editor.GamePieceLayers.property_layer"), //$NON-NLS-1$ // "property name for layer"
        Resources.getString("Editor.GamePieceLayers.order_layer"),    //$NON-NLS-1$ // "layer order"
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,   // This attribute contains a property name. Pieces are then queried for their value of that property, and the value is used to determine what visual layer they go in
      String[].class  // This attribute contains an array of possible values for the previously named property, sorted in the relative visual order in which they should be drawn.
    };
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      PROPERTY_NAME,
      LAYER_ORDER
    };
  }

  @Override
  public String getAttributeValueString(String key) {
    if (PROPERTY_NAME.equals(key)) {
      return collection.propertyName;
    }
    else if (LAYER_ORDER.equals(key)) {
      return StringArrayConfigurer.arrayToString(collection.layerOrder);
    }
    else {
      return null;
    }
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (PROPERTY_NAME.equals(key)) {
      collection.propertyName = (String) value;
    }
    else if (LAYER_ORDER.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      collection.layerOrder = (String[]) value;
      collection.initLayers(collection.layerOrder.length + 1);
    }
  }

  @Override
  public void addTo(Buildable parent) {
    map = (Map)parent;
    validator = new SingleChildInstance(map, getClass());
    map.setPieceCollection(collection);
    if (tempToolBar != null) {
      tempToolBar.setDelegate(map);
    }
  }

  public JToolBar getToolBar() {
    if (tempToolBar == null) {
      tempToolBar = new TemporaryToolBar();
      if (map != null) {
        tempToolBar.setDelegate(map);
      }
    }
    return tempToolBar.getToolBar();
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] { LayerControl.class };
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GamePieceLayers.html"); //NON-NLS
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.GamePieceLayers.component_type"); //$NON-NLS-1$
  }

  @Override
  public void removeFrom(Buildable parent) {
    map.setPieceCollection(new DefaultPieceCollection());
    for (final LayerControl lc : this.getComponentsOf(LayerControl.class)) {
      lc.removeFrom(this);
    }
  }

  public Map getMap() {
    return map;
  }

  public CompoundPieceCollection getPieceCollection() {
    return collection;
  }

  /**
   * The PieceCollection class used by the {@link Map} to which a LayeredPieceCollection has been added. This replaces the
   * simpler {@link DefaultPieceCollection} that the map would ordinarily use. Here we extend {@link CompoundPieceCollection}
   * which maintains a list of pieces currently on each layer as well as a set of flags for whether each layer is presently
   * enabled/visible. We simply provide the methods needed to determine which relative layer (i.e. internally-maintained
   * drawing order) a particular piece (or stack) should be grouped with, by querying the designated property of the
   * piece. In the case of a {@link Stack} we use its memory of the most recent layer it was in, if a piece is not
   * available.
   */
  public static class Collection extends CompoundPieceCollection implements DeckVisitor {
    private String propertyName;
    private String[] layerOrder;
    private final DeckVisitorDispatcher dispatcher = new DeckVisitorDispatcher(this);

    public Collection(String propertyName, String[] layerOrder) {
      super(0);
      setPropertyName(propertyName);
      setLayerOrder(layerOrder);
    }

    public String[] getLayerOrder() {
      return layerOrder;
    }

    public void setLayerOrder(String[] layerOrder) {
      this.layerOrder = layerOrder;
      initLayers(layerOrder.length + 1);
    }

    public String getPropertyName() {
      return propertyName;
    }

    public void setPropertyName(String propertyName) {
      this.propertyName = propertyName;
    }

    /**
     * Gets the appropriate layer for the given piece (or Stack or Deck). Will call one of the dispatcher
     * methods below, as appropriate: {@link #visitDeck}, {@link #visitStack}, or {@link #visitDefault}.
     * @param p Piece (possibly a Deck or Stack) to obtain layer for
     * @return visual layer
     */
    @Override
    public int getLayerForPiece(GamePiece p) {
      return (Integer) dispatcher.accept(p); // This will send to visitDeck/visitStack/visitDefault below.
    }

    @Override
    public int getLayerForName(String layer) {
      for (int i = 0; i < layerOrder.length; i++) {
        if (layer.equals(layerOrder[i])) {
          return i;
        }
      }
      return -1;
    }

    @Override
    public String getLayerNameForPiece(GamePiece p) {
      final int layer = getLayerForPiece(p);
      return layer >= layerOrder.length ? "" : layerOrder[layer];
    }

    @Override
    protected boolean canPiecesMerge(GamePiece p1, GamePiece p2) {
      return super.canPiecesMerge(p1, p2)
          && getLayerForPiece(p1) == getLayerForPiece(p2);
    }

    /**
     * Decks are always displayed in the highest possible layer
     * @param d Deck
     * @return layer - the highest possible one
     */
    @Override
    public Object visitDeck(Deck d) {
      return layerOrder.length;
    }

    /**
     * Ordinary pieces are queried for their value of the designated layer property, and this
     * value is then checked against the list of layer names. If there is no match, the "highest possible layer"
     * is used as the default.
     * @param p ordinary piece
     * @return layer index for this piece
     */
    @Override
    public Object visitDefault(GamePiece p) {
      final String property = (String) p.getProperty(propertyName);
      int layer = layerOrder.length;
      for (int i = 0; i < layerOrder.length; ++i) {
        if (layerOrder[i].equals(property)) {
          layer = i;
          break;
        }
      }

      return layer;
    }

    /**
     * Stacks are first checked for the "topPiece()", but since this method actually hides from us pieces hidden
     * from the currently active player by the {@link VASSAL.counters.Hideable} ("Invisible") trait, we fall back
     * on the Stack's own memory of what layer it has been in.
     * @param s Stack to check layer
     * @return layer index
     */
    @Override
    public Object visitStack(Stack s) {
      final GamePiece top = s.topPiece(); //NOTE: top VISIBLE piece. Can return null for a non-empty stack!
      if (top == null) {
        final int layer = s.getLayer();
        if ((layer != LAYER_NOT_SET) && s.getPieceCount() > 0) {
          final GamePiece secretTop = s.getPieceAt(0);
          if (secretTop != null) {
            return visitDefault(secretTop);
          }
        }
        return layerOrder.length;
      }
      return visitDefault(top);
    }
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Property Names referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    final List<String> l = new ArrayList<>();
    l.add(collection.propertyName);
    l.addAll(Arrays.asList(collection.layerOrder));
    return l;
  }
}
