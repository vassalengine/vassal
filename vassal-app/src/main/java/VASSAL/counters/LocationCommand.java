/*
 * Copyright (c) 2023 by The VASSAL Development team, Brian Reynolds
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
package VASSAL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.Region;
import VASSAL.build.module.map.boardPicker.board.RegionGrid;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.Command;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.PropertyExpressionConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.script.expression.AuditTrail;
import VASSAL.search.SearchTarget;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

import javax.swing.KeyStroke;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * Designates the piece as "Cargo", which can be placed on a "Mat" to move along with it
 */
public class LocationCommand extends Decorator implements TranslatablePiece {
  public static final String ID = "locCommand;"; // NON-NLS

  public static final String LOC_NAME = "LocationOfCommand";

  public static final String LOC_REGIONS = "locRegions"; //NON-NLS
  public static final String LOC_ZONES = "locZones"; //NON-NLS
  public static final String[] LOC_OPTIONS = {LOC_REGIONS, LOC_ZONES};
  public static final String[] LOC_KEYS = {"Editor.LocationCommand.regions", "Editor.LocationCommand.zones"};


  // Type variables (configured in Ed)
  protected String desc;
  protected String locType;
  protected PropertyExpression propertiesFilter = new PropertyExpression("");
  protected FormattedString menuText = new FormattedString("");
  protected NamedKeyStroke key;

  // Private stuff
  private List<LocationKeyCommand> keyCommands = new ArrayList<>();
  private LocationPropertySource locPS = new LocationPropertySource();
  private String evalLocation = "";
  private boolean everBuilt = false;

  private class LocationKeyCommand extends KeyCommand {
    private String locationName;

    public LocationKeyCommand(String name, NamedKeyStroke key, GamePiece target, TranslatablePiece i18nPiece, String locationName) {
      super(name, key, target, i18nPiece);
      this.locationName = locationName;
    }

    public String getLocationName() {
      return locationName;
    }

    public void actionPerformed(ActionEvent evt) {
      final GameModule gm = GameModule.getGameModule();
      gm.setLocationKeyCommand(this);
      super.actionPerformed(evt);
      gm.setLocationKeyCommand(null);
    }
  }

  /**
   * Makes our location-currently-being-evaluated available to property evaluation; other than that, properties from the piece as usual
   */
  private class LocationPropertySource implements PropertySource {
    public Object getProperty(Object key) {
      if (LOC_NAME.equals(key)) {
        return evalLocation;
      }
      else {
        return Decorator.getOutermost(piece).getProperty(key);
      }
    }

    @Override
    public Object getLocalizedProperty(Object key) {
      return getProperty(key);
    }
  }


  public LocationCommand() {
    this(ID + ";", null); //NON-NLS
  }

  public LocationCommand(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }


  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    desc = st.nextToken("");
    locType = st.nextToken(LOC_REGIONS);
    propertiesFilter.setExpression(st.nextToken(""));
    menuText.setFormat(st.nextToken(Resources.getString("Editor.LocationCommand.loc_default_command")));
    key = st.nextNamedKeyStroke();
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(desc)
      .append(locType)
      .append(propertiesFilter.getExpression())
      .append(menuText.getFormat())
      .append(key);
    return ID + se.getValue();
  }

  @Override
  public void mySetState(String newState) {
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(key);
  }

  /**
   * {@link SearchTarget}
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return Arrays.asList(menuText.getFormat());
  }

  /**
   * {@link SearchTarget}
   * @return a list of the Decorator's string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    return Arrays.asList(propertiesFilter.getExpression());
  }


  private void tryName(String name) {
    evalLocation = name;
    if (propertiesFilter.isTrue(locPS)) {
      final AuditTrail audit = AuditTrail.create(this, menuText.getFormat(), Resources.getString("Editor.LocationCommand.evaluate_menu_text"));
      final String myMenuText = menuText.getLocalizedText(this, this, audit);
      keyCommands.add(new LocationKeyCommand(myMenuText, key, Decorator.getOutermost(this), this, evalLocation));
    }
  }

  private void tryZone(Zone zone) {
    tryName(zone.getName());
  }

  private void tryRegion(Region region) {
    tryName(region.getName());
  }

  private void tryGrid(MapGrid grid) {
    if (grid instanceof ZonedGrid) {
      for (final Zone zone : ((ZonedGrid) grid).getZonesList()) {
        if (LOC_ZONES.equals(locType)) {
          tryZone(zone);
          continue;
        }

        tryGrid(zone.getGrid());
      }
    }
    else if ((grid instanceof RegionGrid) && LOC_REGIONS.equals(locType)) {
      for (final Region r : ((RegionGrid)grid).getRegionList().values()) {
        tryRegion(r);
      }
    }
  }


  @Override
  public KeyCommand[] myGetKeyCommands() {
    if ((key == null) || key.isNull()) {
      return KeyCommand.NONE;
    }

    everBuilt = true;

    keyCommands.clear();

    final Map map = getMap();
    if (map == null) return KeyCommand.NONE;
    for (final Board board : map.getBoardPicker().getSelectedBoards()) {
      tryGrid(board.getGrid());
    }

    return (KeyCommand[]) keyCommands.toArray();
  }


  @Override
  public String myGetState() {
    final SequenceEncoder se = new SequenceEncoder(';');
    //se.append(mat == null ? NO_MAT : mat.getId());
    return se.getValue();
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    if (key.equals(stroke)) { //N.B. the usual KeyStroke / NamedKeyStroke shenanigans
      KeyCommand kc = GameModule.getGameModule().getLocationKeyCommand();
      if (!(kc instanceof LocationKeyCommand)) {
        if (!everBuilt) {
          myGetKeyCommands();
        }
        if (keyCommands.isEmpty()) {
          return null;
        }
        kc = keyCommands.get(0);
      }

      // Off to the scratchpad you go!
      setProperty(LOC_NAME, ((LocationKeyCommand) kc).locationName);
    }

    return null;
  }


  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public Object getProperty(Object key) {
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    return super.getLocalizedProperty(key);
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.LocationCommand.trait_description", desc);
  }

  @Override
  public String getBaseDescription() {
    return Resources.getString("Editor.LocationCommand.trait_description");
  }

  @Override
  public String getDescriptionField() {
    return desc;
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof LocationCommand)) return false;
    final LocationCommand c = (LocationCommand) o;
    if (!Objects.equals(desc, c.desc)) return false;
    if (!Objects.equals(locType, c.locType)) return false;
    if (!Objects.equals(propertiesFilter.getExpression(), c.propertiesFilter.getExpression())) return false;
    if (!Objects.equals(menuText.getFormat(), c.menuText.getFormat())) return false;
    return Objects.equals(key, c.key);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("LocationCommand.html"); // NON-NLS
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    return Arrays.asList(LOC_NAME);
  }

  public static class Ed implements PieceEditor {
    private final StringConfigurer descInput;
    private final TranslatingStringEnumConfigurer locTypeInput;
    private PropertyExpressionConfigurer propertyMatchInput;
    private FormattedStringConfigurer menuTextInput;
    private NamedHotKeyConfigurer keyInput;

    private final TraitConfigPanel controls;

    public Ed(LocationCommand p) {
      controls = new TraitConfigPanel();

      descInput = new StringConfigurer(p.desc);
      descInput.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", descInput);

      locTypeInput = new TranslatingStringEnumConfigurer(LOC_OPTIONS, LOC_KEYS);
      locTypeInput.setValue(LOC_REGIONS);
      for (final String s : LOC_OPTIONS) {
        if (s.equals(p.locType)) {
          locTypeInput.setValue(s);
        }
      }
      controls.add("Editor.LocationCommand.location_type", locTypeInput);

      propertyMatchInput = new PropertyExpressionConfigurer(p.propertiesFilter);
      controls.add("Editor.LocationCommand.matching_properties", propertyMatchInput);

      menuTextInput = new FormattedExpressionConfigurer(p.menuText.getFormat(), p);
      controls.add("Editor.LocationCommand.menu_format", menuTextInput);

      keyInput = new NamedHotKeyConfigurer(p.key);
      controls.add("Editor.LocationCommand.key_command", keyInput);
    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(descInput.getValueString())
        .append(locTypeInput.getValueString())
        .append(propertyMatchInput.getValueString())
        .append(menuTextInput.getValueString())
        .append(keyInput.getValueString());

      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }
}
