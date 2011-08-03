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

/**
 * Defines PieceCollection in which pieces are assigned to an arbitrary number of layers
 * according to a property setting
 */
public class LayeredPieceCollection extends AbstractConfigurable {
  public static final String PROPERTY_NAME="property";
  public static final String LAYER_ORDER="layerOrder";
  protected Collection collection = new Collection("Layer", new String[0]);
  protected Map map;
  protected TemporaryToolBar tempToolBar;

  public LayeredPieceCollection() {
    this.setAttributeTranslatable(PROPERTY_NAME, false);
  }

  public String[] getAttributeDescriptions() {
    return new String[]{
        Resources.getString("Editor.GamePieceLayers.property_layer"), //$NON-NLS-1$
        Resources.getString("Editor.GamePieceLayers.order_layer"), //$NON-NLS-1$
    };
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      String[].class
    };
  }

  public String[] getAttributeNames() {
    return new String[]{
      PROPERTY_NAME,
      LAYER_ORDER
    };
  }

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

  public void setAttribute(String key, Object value) {
    if (PROPERTY_NAME.equals(key)) {
      collection.propertyName = (String) value;
    }
    else if (LAYER_ORDER.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      collection.layerOrder = (String[]) value;
      collection.initLayers(collection.layerOrder.length+1);
    }
  }

  public void addTo(Buildable parent) {
    map = (Map)parent;
    validator = new SingleChildInstance(map,getClass());
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

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] { LayerControl.class };
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GamePieceLayers.htm");
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.GamePieceLayers.component_type"); //$NON-NLS-1$
  }

  public void removeFrom(Buildable parent) {
    map.setPieceCollection(new DefaultPieceCollection());
    for (LayerControl lc : this.getComponentsOf(LayerControl.class)) {
      lc.removeFrom(this);
    }
  }

  public Map getMap() {
    return map;
  }

  public CompoundPieceCollection getPieceCollection() {
    return collection;
  }

  /** The PieceCollection class used by the map to which a LayeredPieceCollection has been added */
  public static class Collection extends CompoundPieceCollection implements DeckVisitor {
    private String propertyName;
    private String[] layerOrder;
    private DeckVisitorDispatcher dispatcher = new DeckVisitorDispatcher(this);

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
      initLayers(layerOrder.length+1);
    }

    public String getPropertyName() {
      return propertyName;
    }

    public void setPropertyName(String propertyName) {
      this.propertyName = propertyName;
    }

    public int getLayerForPiece(GamePiece p) {
      return ((Integer)dispatcher.accept(p)).intValue();
    }

    public int getLayerForName(String layer) {
      for (int i=0; i < layerOrder.length; i++) {
        if (layer.equals(layerOrder[i])) {
          return i;
        }
      }
      return -1;
    }

    public String getLayerNameForPiece(GamePiece p) {
      int layer = getLayerForPiece(p);
      return layer >= layerOrder.length ? "" : layerOrder[layer];
    }

    protected boolean canPiecesMerge(GamePiece p1, GamePiece p2) {
      return super.canPiecesMerge(p1, p2)
          && getLayerForPiece(p1) == getLayerForPiece(p2);
    }

    public Object visitDeck(Deck d) {
      return layerOrder.length;
    }

    public Object visitDefault(GamePiece p) {
      String property = (String) p.getProperty(propertyName);
      int layer = layerOrder.length;
      for (int i=0;i<layerOrder.length;++i) {
        if (layerOrder[i].equals(property)) {
          layer = i;
          break;
        }
      }

      return layer;
    }

    public Object visitStack(Stack s) {
      GamePiece top = s.topPiece();
      if (top == null) {
        return layerOrder.length;
      }
      return visitDefault(top);
    }
  }
}
