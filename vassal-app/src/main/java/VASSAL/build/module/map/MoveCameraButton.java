/*
 *
 * Copyright (c) 2021 by Brian Reynolds & VASSAL Team
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

import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.BadDataReport;
import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.TranslatableStringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.SendToLocation;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.AuditTrail;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.FormattedString;
import org.apache.commons.lang3.ArrayUtils;

import java.awt.Point;
import java.util.List;

/**
 * Adds a button to a Maps toolbar that moves the view/camera to a specific point
 */
public class MoveCameraButton extends AbstractToolbarItem {
  public static String MOVE_CAMERA_MODE = "moveCameraMode"; //NON-NLS
  public static String ZOOM = "zoom"; // NON-NLS
  public static String BOARD_NAME = "boardName"; //NON-NLS
  public static String X_POS = "xPos"; //NON-NLS;
  public static String Y_POS = "yPos"; //NON-NLS;
  public static String ZONE_NAME = "zoneName"; //NON-NLS;
  public static String GRID_LOCATION = "gridLocation"; //NON-NLS;
  public static String REGION_NAME = "regionName"; //NON-NLS
  public static String PROPERTY_FILTER = "propertyFilter"; //NON-NLS
  public static String X_OFFSET = "xOffset"; //NON-NLS
  public static String Y_OFFSET = "yOffset"; //NON-NLS

  protected String moveCameraMode = SendToLocation.DEST_LOCATION;
  protected FormattedString zoom = new FormattedString("");
  protected FormattedString board = new FormattedString("");
  protected FormattedString x = new FormattedString("");
  protected FormattedString y = new FormattedString("");
  protected FormattedString gridLocation = new FormattedString("");
  protected FormattedString zone = new FormattedString("");
  protected FormattedString region = new FormattedString("");
  protected FormattedString xOffset = new FormattedString("0");
  protected FormattedString yOffset = new FormattedString("0");
  protected PropertyExpression propertyFilter = new PropertyExpression("");

  protected Map map;

  public static class EmptyFormatConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedExpressionConfigurer(key, name, new String[] { });
    }
  }

  public MoveCameraButton() {
    setNameKey("");

    setLaunchButton(makeLaunchButton(
      Resources.getString("Editor.MoveCameraButton.move_camera_tooltip"),
      Resources.getString("Editor.MoveCameraButton.move_camera"),
      "/images/recenter.gif", //NON-NLS
      e -> moveCamera()
    ));
  }

  private void offsetDest(Point dest) {
    final String dxString = xOffset.getText(map, "0", this, AuditTrail.create(this, xOffset.getFormat(), "X Offset")); //NON-NLS
    final String dyString = xOffset.getText(map, "0", this, AuditTrail.create(this, yOffset.getFormat(), "Y Offset")); //NON-NLS

    final int dx;
    final int dy;
    try {
      dx = Integer.parseInt(dxString);
    }
    catch (NumberFormatException e) {
      ErrorDialog.dataWarning(new BadDataReport(this, Resources.getString("Error.non_number_error"), xOffset.debugInfo(dxString, "X Offset"))); //NON-NLS
      return;
    }

    try {
      dy = Integer.parseInt(dyString);
    }
    catch (NumberFormatException e) {
      ErrorDialog.dataWarning(new BadDataReport(this, Resources.getString("Error.non_number_error"), yOffset.debugInfo(dyString, "Y Offset"))); //NON-NLS
      return;
    }

    dest.x += dx;
    dest.y += dy;
  }

  private Point getDestination() {
    final SendToLocation.Destination dest = SendToLocation.getSendLocation(map, this, moveCameraMode, null, board, zone, region, gridLocation, x, y, propertyFilter, map, map.getCenter());

    if (dest.point != null) {
      offsetDest(dest.point);
    }

    return dest.point;
  }

  private void moveCamera() {
    final Point dest = getDestination();
    if (dest != null) {
      final String zoomString = zoom.getText(map, this, "Editor.MoveCameraButton.zoom");
      if (!zoomString.strip().isEmpty()) {
        try {
          final double toZoom = Double.parseDouble(zoomString);
          if (toZoom > 0) {
            final Zoomer zoomer = map.getZoomer();
            if (zoomer != null) {
              zoomer.setZoomFactor(toZoom);
            }
          }
        }
        catch (NumberFormatException e) {
          ErrorDialog.dataWarning(new BadDataReport(this, Resources.getString("Error.non_number_error"), Resources.getString("Editor.MoveCameraButton.zoom") + ": " + zoomString, e));
        }
      }

      map.centerAt(dest);
    }
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.MoveCameraButton.component_type");
  }

  @Override
  public void addTo(Buildable parent) {
    map = (Map)parent;
    map.getToolBar().add(getLaunchButton());
  }

  public static class DestConfig extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return SendToLocation.DEST_OPTIONS;
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return SendToLocation.DEST_KEYS;
    }
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String key) {
    if (List.of(MOVE_CAMERA_MODE, X_OFFSET, Y_OFFSET, ZOOM).contains(key)) {
      return () -> true;
    }
    else if (BOARD_NAME.equals(key)) {
      return () -> (SendToLocation.DEST_GRIDLOCATION.equals(moveCameraMode) || SendToLocation.DEST_LOCATION.equals(moveCameraMode));
    }
    else if (ZONE_NAME.equals(key)) {
      return () -> SendToLocation.DEST_ZONE.equals(moveCameraMode);
    }
    else if (REGION_NAME.equals(key)) {
      return () -> SendToLocation.DEST_REGION.equals(moveCameraMode);
    }
    else if (GRID_LOCATION.equals(key)) {
      return () -> SendToLocation.DEST_GRIDLOCATION.equals(moveCameraMode);
    }
    else if (X_POS.equals(key) || Y_POS.equals(key)) {
      return () -> SendToLocation.DEST_GRIDLOCATION.equals(moveCameraMode) || SendToLocation.DEST_LOCATION.equals(moveCameraMode);
    }
    else if (PROPERTY_FILTER.equals(key)) {
      return () -> SendToLocation.DEST_COUNTER.equals(moveCameraMode) || SendToLocation.DEST_COUNTER_CYCLE.equals(moveCameraMode) || SendToLocation.DEST_COUNTER_NEAREST.equals(moveCameraMode);
    }
    else {
      return super.getAttributeVisibility(key);
    }
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.addAll(
      super.getAttributeDescriptions(),
      Resources.getString("Editor.MoveCameraButton.move_camera_mode"),
      Resources.getString("Editor.MoveCameraButton.board"),
      Resources.getString("Editor.MoveCameraButton.zone"),
      Resources.getString("Editor.MoveCameraButton.region"),
      Resources.getString("Editor.MoveCameraButton.grid"),
      Resources.getString("Editor.MoveCameraButton.x"),
      Resources.getString("Editor.MoveCameraButton.y"),
      Resources.getString("Editor.MoveCameraButton.x_offset"),
      Resources.getString("Editor.MoveCameraButton.y_offset"),
      Resources.getString("Editor.MoveCameraButton.property_filter"),
      Resources.getString("Editor.MoveCameraButton.zoom")
      );
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(
      super.getAttributeNames(),
      MOVE_CAMERA_MODE,
      BOARD_NAME,
      ZONE_NAME,
      REGION_NAME,
      GRID_LOCATION,
      X_POS,
      Y_POS,
      X_OFFSET,
      Y_OFFSET,
      PROPERTY_FILTER,
      ZOOM
      );
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.addAll(
      super.getAttributeTypes(),
      DestConfig.class,
      EmptyFormatConfig.class,
      EmptyFormatConfig.class,
      EmptyFormatConfig.class,
      EmptyFormatConfig.class,
      EmptyFormatConfig.class,
      EmptyFormatConfig.class,
      EmptyFormatConfig.class,
      EmptyFormatConfig.class,
      PropertyExpression.class,
      EmptyFormatConfig.class
    );
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (MOVE_CAMERA_MODE.equals(key)) {
      if (value instanceof String) {
        moveCameraMode = (String)value;
      }
    }
    else if (BOARD_NAME.equals(key)) {
      board.setFormat((String) value);
    }
    else if (ZONE_NAME.equals(key)) {
      zone.setFormat((String)value);
    }
    else if (REGION_NAME.equals(key)) {
      region.setFormat((String)value);
    }
    else if (GRID_LOCATION.equals(key)) {
      gridLocation.setFormat((String)value);
    }
    else if (X_POS.equals(key)) {
      x.setFormat((String)value);
    }
    else if (Y_POS.equals(key)) {
      y.setFormat((String)value);
    }
    else if (X_OFFSET.equals(key)) {
      xOffset.setFormat((String)value);
    }
    else if (Y_OFFSET.equals(key)) {
      yOffset.setFormat((String)value);
    }
    else if (PROPERTY_FILTER.equals(key)) {
      propertyFilter.setExpression((String)value);
    }
    else if (ZOOM.equals(key)) {
      zoom.setFormat((String)value);
    }
    else {
      super.setAttribute(key, value);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (MOVE_CAMERA_MODE.equals(key)) {
      return moveCameraMode;
    }
    else if (BOARD_NAME.equals(key)) {
      return board.getFormat();
    }
    else if (ZONE_NAME.equals(key)) {
      return zone.getFormat();
    }
    else if (REGION_NAME.equals(key)) {
      return region.getFormat();
    }
    else if (GRID_LOCATION.equals(key)) {
      return gridLocation.getFormat();
    }
    else if (X_POS.equals(key)) {
      return x.getFormat();
    }
    else if (Y_POS.equals(key)) {
      return y.getFormat();
    }
    else if (X_OFFSET.equals(key)) {
      return xOffset.getFormat();
    }
    else if (Y_OFFSET.equals(key)) {
      return yOffset.getFormat();
    }
    else if (PROPERTY_FILTER.equals(key)) {
      return propertyFilter.getExpression();
    }
    else if (ZOOM.equals(key)) {
      return zoom.getFormat();
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.html", "MoveCameraButton"); //NON-NLS
  }

  @Override
  public void removeFrom(Buildable parent) {
    map.getToolBar().remove(getLaunchButton());
  }
}
