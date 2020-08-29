/*
 *
 * Copyright (c) 2020 by Vassal developers
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

import javax.swing.JOptionPane;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import com.fasterxml.jackson.core.JsonEncoding;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;

import VASSAL.Info;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.PropertyNameSource;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.filechooser.FileChooser;

public final class JsonSaver extends AbstractConfigurable {

  private static final String HOTKEY = "hotkey";
  private static final String BUTTON_TEXT = "buttonText";
  private static final String TOOLTIP = "tooltip";
  private static final String ICON_NAME = "icon";

  private Map map;
  private final LaunchButton launch;

  public JsonSaver() {
    ActionListener al = e -> apply();
    launch = new LaunchButton("Save JSON", TOOLTIP, BUTTON_TEXT, HOTKEY, ICON_NAME, al);
    launch.setAttribute(TOOLTIP, "Save map contents as JSON file");
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] {
      BUTTON_TEXT,
      TOOLTIP,
      ICON_NAME,
      HOTKEY
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      Resources.getString(Resources.BUTTON_TEXT),
      Resources.getString(Resources.TOOLTIP_TEXT),
      Resources.getString(Resources.BUTTON_ICON),
      Resources.getString(Resources.HOTKEY_LABEL),
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      String.class,
      JsonSaver.IconConfig.class,
      NamedKeyStroke.class
    };
  }

  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((JsonSaver) c).launch.getAttributeValueString(ICON_NAME));
    }
  }

  @Override
  public void setAttribute(String key, Object value) {
    launch.setAttribute(key, value);
  }

  @Override
  public String getAttributeValueString(String key) {
    return launch.getAttributeValueString(key);
  }

  @Override
  public void addTo(Buildable b) {
    map = (Map) b;
    map.getToolBar().add(launch);
  }

  @Override
  public void removeFrom(Buildable b) {
    map = (Map) b;
    map.getToolBar().remove(launch);
    map.getToolBar().revalidate();
  }

  public void apply() {
    ExportOption exportOption = determineExportOption();
    writeMapAsJson(exportOption);
  }

  private ExportOption determineExportOption() {
    final int showVisibleToEveryoneResult = JOptionPane.showConfirmDialog(
      null,
      "Write all visible contents as seen by everyone?",
      "",
      JOptionPane.YES_NO_OPTION);
    return showVisibleToEveryoneResult == JOptionPane.NO_OPTION
      ? ExportOption.VISIBLE_TO_ME
      : ExportOption.VISIBLE_TO_EVERYONE;
  }

  private void writeMapAsJson(ExportOption exportOption) {
    final FileChooser fc = GameModule.getGameModule().getFileChooser();
    if (fc.showSaveDialog(map.getView()) != FileChooser.APPROVE_OPTION) return;

    final File file =  fc.getSelectedFile();

    try (FileOutputStream fos = new FileOutputStream(file)) {
      JsonFactory jsonFactory = new JsonFactory();
      try (JsonGenerator jsonGenerator = jsonFactory.createGenerator(fos, JsonEncoding.UTF8).useDefaultPrettyPrinter()) {
        doWriteMap(jsonGenerator, exportOption);
      }
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, file);
    }
  }

  private void doWriteMap(JsonGenerator jsonGenerator, ExportOption exportOption) throws IOException {
    jsonGenerator.writeStartObject();
    jsonGenerator.writeStringField("_version", Info.getVersion());

    final List<Map> maps = Map.getMapList();
    for (Map aMap : maps) {
      final java.util.Map<String, List<PieceDTO>> pieceMap = buildPieceMap(aMap, exportOption);
      jsonGenerator.writeObjectFieldStart(aMap.getMapName());
      writeMapContents(jsonGenerator, pieceMap);
      jsonGenerator.writeEndObject();
    }

    jsonGenerator.writeEndObject();
  }

  private static void writeMapContents(JsonGenerator jsonGenerator, java.util.Map<String, List<PieceDTO>> pieceMap)
    throws IOException {

    for (java.util.Map.Entry<String, List<PieceDTO>> entry : pieceMap.entrySet()) {
      String position = entry.getKey();
      List<PieceDTO> pieces = entry.getValue();
      jsonGenerator.writeArrayFieldStart(position);
      for (PieceDTO piece : pieces) {
        jsonGenerator.writeStartObject();
        jsonGenerator.writeStringField("_name", piece.getName());
        for (java.util.Map.Entry<String, String> e : piece.getProperties().entrySet()) {
          final String value = e.getValue();
          if (StringUtils.isNumeric(value)) {
            jsonGenerator.writeNumberField(e.getKey(), new BigDecimal(value));
          }
          else if (value.equalsIgnoreCase("null")) {
            jsonGenerator.writeNullField(e.getKey());
          }
          else if (value.equalsIgnoreCase("true") || value.equalsIgnoreCase("false")) {
            jsonGenerator.writeBooleanField(e.getKey(), Boolean.parseBoolean(value));
          }
          else {
            jsonGenerator.writeStringField(e.getKey(), value);
          }
        }
        jsonGenerator.writeEndObject();
      }
      jsonGenerator.writeEndArray();
    }
  }

  private static java.util.Map<String, List<PieceDTO>> buildPieceMap(Map aMap, ExportOption exportOption) {
    String myId = null;
    if (exportOption == ExportOption.VISIBLE_TO_EVERYONE) {
      myId = GameModule.getUserId();
      GameModule.setUserId("randomUserId357246");
    }
    java.util.Map<String, List<PieceDTO>> result = new HashMap<>();
    final GamePiece[] pieces = aMap.getPieces();

    final java.util.Map<Boolean, List<GamePiece>> piecesStackMap = Arrays
      .stream(pieces)
      .collect(Collectors.partitioningBy(gamePiece -> gamePiece instanceof Stack));

    addGamePieceProperties(result, aMap, piecesStackMap.get(Boolean.FALSE));
    piecesStackMap.get(Boolean.TRUE).forEach(gamePiece -> addGamePieceProperties(result, aMap, ((Stack)gamePiece).asList()));

    if (exportOption == ExportOption.VISIBLE_TO_EVERYONE) {
      GameModule.setUserId(myId);
    }
    return result;
  }

  private static void addGamePieceProperties(java.util.Map<String, List<PieceDTO>> result, Map aMap,
                                             List<GamePiece> gamePieces) {
    for (GamePiece gp : gamePieces) {
      if (gp.getName().isEmpty()) {
        continue;
      }

      result.merge(
        aMap.locationName(gp.getPosition()),
        new ArrayList<>(List.of(toPieceDTO(gp))),
        (pieceDTOS, pieceDTOS2) -> {
          pieceDTOS.addAll(pieceDTOS2);
          return pieceDTOS;
        });
    }
  }

  private static PieceDTO toPieceDTO(GamePiece gamePiece) {
    return new PieceDTO(gamePiece.getName(), buildProperties(gamePiece));
  }

  private static java.util.Map<String, String> buildProperties(GamePiece gamePiece) {
    java.util.Map<String, String> result = new HashMap<>();
    if (gamePiece instanceof PropertyNameSource) {
      PropertyNameSource pns = (PropertyNameSource)gamePiece;
      final List<String> propertyNames = pns.getPropertyNames();
      for (String propertyName : propertyNames) {
        result.put(propertyName, String.valueOf(gamePiece.getProperty(propertyName)));
      }
    }

    if (gamePiece instanceof Decorator) {
      result.putAll(buildProperties(((Decorator) gamePiece).getInner()));
    }

    return result;
  }

  @Override
  public HelpFile getHelpFile() {
    // TODO add documentation
    return HelpFile.getReferenceManualPage("Map.htm", "TextCapture");
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.JsonCapture.component_type"); //$NON-NLS-1$
  }

  /**
   * @return an array of Configurer objects representing
   * all possible classes of Buildable children of this Configurable object
   */
  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  private enum ExportOption {
    VISIBLE_TO_ME,
    VISIBLE_TO_EVERYONE
  }

  private static class PieceDTO {
    private final String name;
    private final java.util.Map<String, String> properties;

    public PieceDTO(String name, java.util.Map<String, String> properties) {
      this.name = name;
      this.properties = properties;
    }

    public String getName() {
      return name;
    }

    public java.util.Map<String, String> getProperties() {
      return properties;
    }
  }
}
