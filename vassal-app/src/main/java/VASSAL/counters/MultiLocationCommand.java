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
import VASSAL.configure.BooleanConfigurer;
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
public class MultiLocationCommand extends Decorator implements TranslatablePiece {
  public static final String ID = "locCommand;"; // NON-NLS

  // Properties for both naming commands & checking which menu item was picked
  public static final String LOC_NAME = "LocationOfCommand"; //NON-NLS
  public static final String LOC_ZONE = "ZoneOfCommand"; //NON-NLS
  public static final String LOC_BOARD = "BoardOfCommand"; //NON-NLS
  public static final String LOC_MAP   = "MapOfCommand"; //NON-NLS

  public static final String LOC_REGIONS = "locRegions"; //NON-NLS
  public static final String LOC_ZONES = "locZones"; //NON-NLS
  public static final String[] LOC_OPTIONS = {LOC_REGIONS, LOC_ZONES};
  public static final String[] LOC_KEYS = {"Editor.MultiLocationCommand.regions", "Editor.MultiLocationCommand.zones"};

  // Type variables (configured in Ed)
  protected String desc;
  protected String locType;
  protected Boolean curMapOnly = true;
  protected PropertyExpression propertiesFilter = new PropertyExpression("");
  protected FormattedString menuText = new FormattedString("");
  protected NamedKeyStroke key;

  // Private stuff (shhhh!)
  private final List<MultiLocationKeyCommand> keyCommands = new ArrayList<>();
  private final MultiLocationPropertySource locPS = new MultiLocationPropertySource();
  private String evalLocation = "";
  private String evalZone = "";
  private String evalBoard = "";
  private String evalMap = "";
  private boolean everBuilt = false;

  public static class MultiLocationKeyCommand extends KeyCommand {
    private static final long serialVersionUID = 1L;

    private final String locationName;
    private final String zoneName;
    private final String boardName;
    private final String mapName;

    public MultiLocationKeyCommand(String name, NamedKeyStroke key, GamePiece target, TranslatablePiece i18nPiece, String locationName, String zoneName, String boardName, String mapName) {
      super(name, key, target, i18nPiece);
      this.locationName = locationName;
      this.zoneName = zoneName;
      this.boardName = boardName;
      this.mapName = mapName;
    }

    public String getLocationName() {
      return locationName;
    }

    @Override
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
  private class MultiLocationPropertySource implements PropertySource {
    @Override
    public Object getProperty(Object key) {
      if (LOC_NAME.equals(key)) {
        return evalLocation;
      }
      else if (LOC_ZONE.equals(key)) {
        return evalZone;
      }
      else if (LOC_BOARD.equals(key)) {
        return evalBoard;
      }
      else if (LOC_MAP.equals(key)) {
        return evalMap;
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


  public MultiLocationCommand() {
    this(ID + ";", null); //NON-NLS
  }

  public MultiLocationCommand(String type, GamePiece inner) {
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
    menuText.setFormat(st.nextToken(Resources.getString("Editor.MultiLocationCommand.loc_default_command")));
    key = st.nextNamedKeyStroke();
    curMapOnly = st.nextBoolean(true);
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(desc)
      .append(locType)
      .append(propertiesFilter.getExpression())
      .append(menuText.getFormat())
      .append(key)
      .append(curMapOnly);
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
    if (propertiesFilter.getExpression().isBlank() || propertiesFilter.isTrue(locPS)) {
      final AuditTrail audit = AuditTrail.create(locPS, menuText.getFormat(), Resources.getString("Editor.MultiLocationCommand.evaluate_menu_text"));
      final String myMenuText = menuText.getLocalizedText(locPS, this, audit);
      keyCommands.add(new MultiLocationKeyCommand(myMenuText, key, Decorator.getOutermost(this), this, evalLocation, evalZone, evalBoard, evalMap));
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
        evalZone = zone.getName();

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

  private void tryMap(Map map) {
    evalMap = map.getMapName();
    for (final Board board : map.getBoardPicker().getSelectedBoards()) {
      evalBoard = board.getName();
      tryGrid(board.getGrid());
    }
  }


  @Override
  public KeyCommand[] myGetKeyCommands() {
    if ((key == null) || key.isNull()) {
      return KeyCommand.NONE;
    }

    everBuilt = true;

    keyCommands.clear();

    if (curMapOnly) {
      final Map map = getMap();
      if (map == null) return KeyCommand.NONE;
      tryMap(map);
    }
    else {
      for (final Map map : Map.getMapList()) {
        tryMap(map);
      }
    }

    return keyCommands.toArray(new KeyCommand[0]);
  }


  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    if (key.equals(stroke)) { //N.B. the usual KeyStroke / NamedKeyStroke shenanigans
      KeyCommand kc = GameModule.getGameModule().getLocationKeyCommand();
      if (!(kc instanceof MultiLocationKeyCommand)) {
        if (!everBuilt) {
          myGetKeyCommands();
        }
        if (keyCommands.isEmpty()) {
          return null;
        }
        kc = keyCommands.get(0);
      }

      // Off to the scratchpad you go!
      setProperty(LOC_NAME, ((MultiLocationKeyCommand) kc).locationName);
      setProperty(LOC_ZONE, ((MultiLocationKeyCommand) kc).zoneName);
      setProperty(LOC_BOARD, ((MultiLocationKeyCommand) kc).boardName);
      setProperty(LOC_MAP, ((MultiLocationKeyCommand) kc).mapName);
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
    return buildDescription("Editor.MultiLocationCommand.trait_description", desc);
  }

  @Override
  public String getBaseDescription() {
    return Resources.getString("Editor.MultiLocationCommand.trait_description");
  }

  @Override
  public String getDescriptionField() {
    return desc;
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof MultiLocationCommand)) return false;
    final MultiLocationCommand c = (MultiLocationCommand) o;
    if (!Objects.equals(desc, c.desc)) return false;
    if (!Objects.equals(locType, c.locType)) return false;
    if (!Objects.equals(propertiesFilter.getExpression(), c.propertiesFilter.getExpression())) return false;
    if (!Objects.equals(menuText.getFormat(), c.menuText.getFormat())) return false;
    if (!Objects.equals(curMapOnly, c.curMapOnly)) return false;
    return Objects.equals(key, c.key);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("MultiLocationCommand.html"); // NON-NLS
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
    private final BooleanConfigurer curMapOnlyInput;
    private final PropertyExpressionConfigurer propertyMatchInput;
    private final FormattedStringConfigurer menuTextInput;
    private final NamedHotKeyConfigurer keyInput;

    private final TraitConfigPanel controls;

    public Ed(MultiLocationCommand p) {
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
      controls.add("Editor.MultiLocationCommand.location_type", locTypeInput);

      curMapOnlyInput = new BooleanConfigurer(p.curMapOnly);
      controls.add("Editor.MultiLocationCommand.current_map_only", curMapOnlyInput);

      propertyMatchInput = new PropertyExpressionConfigurer(p.propertiesFilter);
      controls.add("Editor.MultiLocationCommand.matching_properties", propertyMatchInput);

      menuTextInput = new FormattedExpressionConfigurer(p.menuText.getFormat(), p);
      controls.add("Editor.MultiLocationCommand.menu_format", menuTextInput);

      keyInput = new NamedHotKeyConfigurer(p.key);
      controls.add("Editor.MultiLocationCommand.key_command", keyInput);
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
        .append(keyInput.getValueString())
        .append(curMapOnlyInput.getValueString());

      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }
}
