/*
 * $Id$
 *
 * Copyright (c) 2000-2006 by Brent Easton, Rodney Kinney
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
package VASSAL.build.module.map.boardPicker.board.mapgrid;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;

/**
 *
 * @author Brent Easton
 * A Container class for Zone Highlighters.
 *
 */
public class ZonedGridHighlighter extends AbstractConfigurable  {

  protected List<ZoneHighlight> highlightList;
  protected String currentColorName;
  protected Color currentColor ;

  public ZonedGridHighlighter() {
    highlightList = new ArrayList<ZoneHighlight>();
  }

  public void addHighlight(ZoneHighlight h) {
    highlightList.add(h);
  }

  public void removeHighlight(ZoneHighlight h) {
    highlightList.remove(h);
  }

  public ZoneHighlight getZoneHighlightByName(String highlightName) {
    for (ZoneHighlight h : highlightList) {
      if (h.getName().equals(highlightName)) {
        return h;
      }
    }
    return null;
  }

  public String getName() {
    return name;
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[0];
  }


  public void addTo(Buildable b) {
    ((ZonedGrid) b).setZoneHighlighter(this);
  }

  public void removeFrom(Buildable b) {
    ((ZonedGrid) b).setZoneHighlighter(null);
  }

  public static String getConfigureTypeName() {
    return "Zone Highlighters";
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ZonedGrid.htm","ZoneHighlighter");
  }

  public String getAttributeValueString(String key) {
    return null;
  }

  public void setAttribute(String key, Object val) {

  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] {ZoneHighlight.class};
  }

}
