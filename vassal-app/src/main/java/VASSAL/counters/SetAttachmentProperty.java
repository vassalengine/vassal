/*
 *
 * Copyright (c) 2000-2023 by The Vassal Development Team, Brent Easton, Rodney Kinney, Brian Reynolds
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

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.PropertyPrompt;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.DynamicKeyCommandListConfigurer;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;

import javax.swing.JLabel;
import javax.swing.KeyStroke;
import java.awt.Component;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Objects;

/**
 *
 * @author Brian Reynolds, Brent Easton
 *
 * A trait that allows counters to manipulate the value of the Dynamic Properties of attached pieces.
 * Uses the Property manipulation functionality of DynamicProperty, but
 * applies them to Dynamic Properties on attached pieces.
 */
public class SetAttachmentProperty extends DynamicProperty {
  protected PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);
  public static final String ID = "setattachprop;"; // NON-NLS
  public static final String INDEX_PROP = "AttachmentIndex"; //NON-NLS
  public static final String ATTACH_PROP = "AttachmentName"; //NON-NLS
  public static final String ATTACH_BASIC = "AttachmentBasicName"; //NON-NLS
  protected String description;
  protected String attachName;
  protected String attachIndex;
  protected Decorator dec;

  private final SetAttachmentPropertySource propertiesWithIndex = new SetAttachmentPropertySource();
  private int index = 0;
  private String currentBasicName = "";
  private String currentAttachmentName = "";
  private final SetAttachmentProperty This = this; // So our property source thing can report our identity downstream

  /**
   * Makes our special location-currently-being-evaluated information available to our property-match evaluation; other than that, properties from the piece as usual
   */
  public class SetAttachmentPropertySource implements PropertySource {
    @Override
    public Object getProperty(Object key) {
      if (INDEX_PROP.equals(key)) {
        return index;
      }
      else if (ATTACH_PROP.equals(key)) {
        return currentAttachmentName;
      }
      else if (ATTACH_BASIC.equals(key)) {
        return currentBasicName;
      }
      else {
        return Decorator.getOutermost(piece).getProperty(key);
      }
    }

    public GamePiece getPiece() {
      return This;
    }

    @Override
    public Object getLocalizedProperty(Object key) {
      return getProperty(key);
    }
  }


  public SetAttachmentProperty() {
    this(ID, null);
  }

  public SetAttachmentProperty(String type, GamePiece p) {
    super(type, p);
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.SetAttachmentProperty.trait_description", key, description) + getCommandsList();
  }

  @Override
  public String getBaseDescription() {
    return Resources.getString("Editor.SetAttachmentProperty.trait_description");
  }

  @Override
  public String getDescriptionField() {
    return description;
  }

  @Override
  public void mySetType(String s) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');
    sd.nextToken(); // Skip over command prefix
    key = sd.nextToken("name");
    decodeConstraints(sd.nextToken(""));
    keyCommandListConfig.setValue(sd.nextToken(""));
    keyCommands = keyCommandListConfig.getListValue().toArray(new DynamicKeyCommand[0]);

    menuCommands = Arrays.stream(keyCommands).filter(
      kc -> !StringUtils.isEmpty(kc.getName())
    ).toArray(KeyCommand[]::new);

    description = sd.nextToken("");
    attachName = sd.nextToken("");
    attachIndex = sd.nextToken("");
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(key);
    se.append(encodeConstraints());
    se.append(keyCommandListConfig.getValueString());
    se.append(description);
    se.append(attachName);
    se.append(attachIndex);
    return ID + se.getValue();
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public void mySetState(String state) {
  }

  /*
   * Duplicate code from Decorator for setProperty(), getProperty() Do not call super.xxxProperty() as we no longer
   * contain a DynamicProperty that can be manipulated, but you cannot call super.super.xxxProperty().
   */
  @Override
  public Object getProperty(Object key) {
    if (Properties.KEY_COMMANDS.equals(key)) {
      return getKeyCommands();
    }
    else if (Properties.INNER.equals(key)) {
      return piece;
    }
    else if (Properties.OUTER.equals(key)) {
      return dec;
    }
    else if (Properties.VISIBLE_STATE.equals(key)) {
      return myGetState() + piece.getProperty(key);
    }
    else if (INDEX_PROP.equals(key)) {
      return index;
    }
    else if (ATTACH_PROP.equals(key)) {
      return currentAttachmentName;
    }
    else if (ATTACH_BASIC.equals(key)) {
      return currentBasicName;
    }
    else {
      return piece.getProperty(key);
    }
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (Properties.KEY_COMMANDS.equals(key)) {
      return getProperty(key);
    }
    else if (Properties.INNER.equals(key)) {
      return getProperty(key);
    }
    else if (Properties.OUTER.equals(key)) {
      return getProperty(key);
    }
    else if (Properties.VISIBLE_STATE.equals(key)) {
      return getProperty(key);
    }
    else if (INDEX_PROP.equals(key)) {
      return index;
    }
    else if (ATTACH_PROP.equals(key)) {
      return currentAttachmentName;
    }
    else if (ATTACH_BASIC.equals(key)) {
      return currentBasicName;
    }
    else {
      return piece.getLocalizedProperty(key);
    }
  }

  @Override
  public void setProperty(Object key, Object val) {
    if (Properties.INNER.equals(key)) {
      setInner((GamePiece) val);
    }
    else if (Properties.OUTER.equals(key)) {
      dec = (Decorator) val;
    }
    else {
      piece.setProperty(key, val);
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("SetAttachmentProperty.html"); // NON-NLS
  }

  /*
   * Locate the correct property/properties to adjust and update value(s).
   * $xxxx$ names are allowed in any of the property name, attachment name, and attachment index fields
   * Blank fields for attachment name and/or index count as wild cards
   */
  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    Command comm = new NullCommand();
    for (final DynamicKeyCommand keyCommand : keyCommands) {
      if (keyCommand.matches(stroke)) {
        final String propertyName = (new FormattedString(key)).getText(Decorator.getOutermost(this), this, "Editor.DynamicProperty.property_name");

        // For evaluating name, we allow either (a) blank means any attachment, (b) string means that attachment name, (c) true/false expression with "AttachmentName" as an available property
        String attachmentName = (new FormattedString(attachName)).getText(propertiesWithIndex, this, "Editor.SetAttachmentProperty.attachment_name");

        // For evaluating index, we allow either: (a) blank means every index, (b) number means 1-based specific index, or (c) true expression with "AttachmentIndex" and/or "AttachmentBasicName" as an available property
        String attachmentIndex = (new FormattedString(attachIndex)).getText(propertiesWithIndex, this, "Editor.SetAttachmentProperty.attachment_index");

        // Find matching attachments on our piece and set their properties
        GamePiece p = Decorator.getOutermost(this);
        while (p instanceof Decorator) {
          if (p instanceof Attachment) {
            final Attachment attachment = (Attachment)p;

            currentAttachmentName = attachment.attachName;

            String newValue = null;

            // When we find an Attachment trait, check attachment Name matches
            boolean match = false;
            if (attachmentName.isBlank() || attachmentName.equals(attachment.getAttachName())) {
              match = true;
            }
            else {
              // In this case only (a special "Attachment" expression), we need to re-evaluate for each attachment
              attachmentName = (new FormattedString(attachName)).getText(propertiesWithIndex, this, "Editor.SetAttachmentProperty.attachment_name");
              match = "true".equals(attachmentName); //NON-NLS
            }

            if (match) {
              index = 0;
              for (final GamePiece propPiece : attachment.getAttachList()) {
                index++; // Intentionally first piece is considered index "1"

                currentBasicName = (String)propPiece.getProperty(BasicPiece.BASIC_NAME);

                if (!attachmentIndex.isBlank()) {
                  if (NumberUtils.isParsable(attachmentIndex)) {
                    final int parse = NumberUtils.toInt(attachmentIndex);
                    if ((parse != 0) && (parse != index)) continue;
                  }
                  else if (!attachmentIndex.equals(currentBasicName)) {
                    // In this case only (a special "Index" expression), we need to re-evaluate for each index
                    attachmentIndex = (new FormattedString(attachIndex)).getText(propertiesWithIndex, this, "Editor.SetAttachmentProperty.attachment_index");
                    if (!"true".equals(attachmentIndex)) { //NON-NLS
                      continue;
                    }
                  }
                }

                GamePiece dp = Decorator.getOutermost(propPiece);
                while (dp instanceof Decorator) {
                  if (dp instanceof DynamicProperty) {
                    final DynamicProperty ourDP = (DynamicProperty)dp;

                    // If we find a matching property, apply our setter on it and we're done
                    if (propertyName.equals(ourDP.key)) {

                      // If we're doing a prompt, prompt only once. Otherwise re-evaluate setter each time
                      if ((newValue == null) || !(keyCommand.propChanger instanceof PropertyPrompt)) {
                        newValue = keyCommand.propChanger.getNewValue(ourDP.value);
                        // The PropertyChanger has already evaluated any Beanshell, only need to handle any remaining $$variables.
                        if (newValue.indexOf('$') >= 0) {
                          format.setFormat(newValue);
                          newValue = format.getText(propertiesWithIndex, this, "Editor.PropertyChangeConfigurer.new_value");
                        }
                      }

                      final ChangeTracker ct = new ChangeTracker(ourDP);

                      ourDP.value = newValue;

                      comm = comm.append(ct.getChangeCommand());
                    }
                  }
                  dp = ((Decorator) dp).getInner();
                }
              }
            }
          }
          p = ((Decorator) p).getInner();
        }
      }
    }
    return comm;
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof SetAttachmentProperty)) return false;
    final SetAttachmentProperty c = (SetAttachmentProperty) o;

    if (! Objects.equals(key, c.key)) return false;
    if (! Objects.equals(encodeConstraints(), c.encodeConstraints())) return false;
    if (! Objects.equals(keyCommandListConfig.getValueString(), c.keyCommandListConfig.getValueString())) return false;
    if (! Objects.equals(description, c.description)) return false;
    if (! Objects.equals(attachName, c.attachName)) return false;
    return Objects.equals(attachIndex, c.attachIndex);
  }

  protected static class Ed implements PieceEditor {
    protected StringConfigurer descConfig;
    protected FormattedExpressionConfigurer nameConfig;
    protected BooleanConfigurer numericConfig;
    protected JLabel minLabel;
    protected IntConfigurer minConfig;
    protected JLabel maxLabel;
    protected IntConfigurer maxConfig;
    protected JLabel wrapLabel;
    protected BooleanConfigurer wrapConfig;
    protected DynamicKeyCommandListConfigurer keyCommandListConfig;
    protected TranslatingStringEnumConfigurer levelConfig;
    protected FormattedExpressionConfigurer attachNameConfig;
    protected FormattedExpressionConfigurer attachIndexConfig;
    protected TraitConfigPanel controls;

    public Ed(final SetAttachmentProperty m) {
      keyCommandListConfig = new DynamicKeyCommandListConfigurer(null, Resources.getString("Editor.DynamicProperty.commands"), m);
      keyCommandListConfig.setValue(new ArrayList<>(Arrays.asList(m.keyCommands)));

      controls = new TraitConfigPanel();

      descConfig = new StringConfigurer(m.description);
      descConfig.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", descConfig);

      nameConfig = new FormattedExpressionConfigurer(m.getKey(), (EditablePiece) m);
      nameConfig.setHintKey("Editor.SetAttachmentProperty.property_name_hint");
      controls.add("Editor.SetAttachmentProperty.property_name", nameConfig);

      attachNameConfig = new FormattedExpressionConfigurer(m.attachName, (EditablePiece) m);
      attachNameConfig.setHintKey("Editor.SetAttachmentProperty.attach_name_hint");
      controls.add("Editor.SetAttachmentProperty.attach_name", attachNameConfig);

      attachIndexConfig = new FormattedExpressionConfigurer(m.attachIndex, (EditablePiece) m);
      attachIndexConfig.setHintKey("Editor.SetAttachmentProperty.attach_index_hint");
      controls.add("Editor.SetAttachmentProperty.attach_index", attachIndexConfig);

      numericConfig = new BooleanConfigurer(m.isNumeric());
      controls.add("Editor.DynamicProperty.is_numeric", numericConfig);

      minLabel = new JLabel(Resources.getString("Editor.GlobalProperty.minimum_value"));
      minConfig = new IntConfigurer(m.getMinimumValue());
      controls.add(minLabel, minConfig);

      maxLabel = new JLabel(Resources.getString("Editor.GlobalProperty.maximum_value"));
      maxConfig = new IntConfigurer(m.getMaximumValue());
      controls.add(maxLabel, maxConfig);

      wrapLabel  = new JLabel(Resources.getString("Editor.DynamicProperty.wrap"));
      wrapConfig = new BooleanConfigurer(m.isWrap());
      controls.add(wrapLabel, wrapConfig);

      controls.add("Editor.DynamicProperty.key_commands", keyCommandListConfig);

      numericConfig.addPropertyChangeListener(evt -> {
        final boolean isNumeric = numericConfig.booleanValue();
        minConfig.getControls().setVisible(isNumeric);
        minLabel.setVisible(isNumeric);
        maxConfig.getControls().setVisible(isNumeric);
        maxLabel.setVisible(isNumeric);
        wrapConfig.getControls().setVisible(isNumeric);
        wrapLabel.setVisible(isNumeric);
        keyCommandListConfig.repack();
      });

      numericConfig.fireUpdate();
    }

    @Override
    public Component getControls() {
      return controls;
    }

    protected String encodeConstraints() {
      return new SequenceEncoder(',')
        .append(numericConfig.getValueString())
        .append(minConfig.getValueString())
        .append(maxConfig.getValueString())
        .append(wrapConfig.getValueString())
        .getValue();
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameConfig.getValueString());
      se.append(encodeConstraints());
      se.append(keyCommandListConfig.getValueString());
      se.append(descConfig.getValueString());
      se.append(attachNameConfig.getValueString());
      se.append(attachIndexConfig.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }
}
