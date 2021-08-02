/*
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
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
package VASSAL.configure;

import VASSAL.counters.PieceAccess;
import VASSAL.counters.PlayerAccess;
import VASSAL.counters.SideAccess;
import VASSAL.counters.SpecifiedSideAccess;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.util.ArrayList;
import java.util.Arrays;

import javax.swing.JPanel;

public class PieceAccessConfigurer extends Configurer {
  protected static final String PLAYER = "player:"; // NON-NLS
  protected static final String SIDE = "side:"; // NON-NLS
  protected static final String SIDES = "sides:"; // NON-NLS
  protected JPanel controls;
  protected String[] prompts = {"Any Player", "Any side", "Any of the specified sides"}; // NON-NLS
  protected String[] promptKeys = {
    "Editor.PieceAccessConfigurer.any_player",
    "Editor.PieceAccessConfigurer.any_side",
    "Editor.PieceAccessConfigurer.any_specified"
  };
  protected TranslatingStringEnumConfigurer selectType;
  protected StringArrayConfigurer sideConfig;

  public PieceAccessConfigurer(String key, String name, PieceAccess value) {
    super(key, name, value);
  }

  public PieceAccessConfigurer(PieceAccess value) {
    this(null, "", value);
  }

  @Override
  public void setValue(Object o) {
    super.setValue(o);
    updateControls();
  }

  @Override
  public String getValueString() {
    return encode(getPieceAccess());
  }

  @Override
  public void setValue(String s) {
    setValue(decode(s));
  }

  @Override
  public Component getControls() {
    if (controls == null) {
      controls = new ConfigurerPanel(getName(), "[fill,grow]", "[][fill,grow]"); // NON-NLS

      selectType = new TranslatingStringEnumConfigurer(prompts, promptKeys);
      selectType.addPropertyChangeListener(e -> {
        updateValue();
        sideConfig.getControls().setVisible(getValue() instanceof SpecifiedSideAccess);
        repack();
      });
      controls.add(selectType.getControls(), "wrap"); // NON-NLS;

      sideConfig = new StringArrayConfigurer("", null);
      sideConfig.setHintKey("Editor.PieceAccessConfigurer.side_hint");
      sideConfig.addPropertyChangeListener(evt -> updateValue());
      controls.add(sideConfig.getControls(), "growx,wrap"); // NON-NLS
      updateControls();
    }
    return controls;
  }

  protected void updateValue() {
    noUpdate = true;
    if (prompts[1].equals(selectType.getValueString())) {
      setValue(SideAccess.getInstance());
    }
    else if (prompts[2].equals(selectType.getValueString())) {
      setValue(new SpecifiedSideAccess(Arrays.asList(sideConfig.getStringArray())));
    }
    else {
      setValue(PlayerAccess.getInstance());
    }
    noUpdate = false;
  }

  protected void updateControls() {
    if (!noUpdate && controls != null) {
      sideConfig.getControls().setVisible(getValue() instanceof SpecifiedSideAccess);
      if (getValue() instanceof SideAccess) {
        selectType.setValue(prompts[1]);
      }
      else if (getValue() instanceof SpecifiedSideAccess) {
        sideConfig.setValue(((SpecifiedSideAccess) getPieceAccess()).getSides().toArray(new String[0]));
        selectType.setValue(prompts[2]);
      }
      else {
        selectType.setValue(prompts[0]);
      }
    }
  }

  public String[] getPrompts() {
    return prompts;
  }

  public PieceAccess getPieceAccess() {
    return (PieceAccess) getValue();
  }

  public static PieceAccess decode(String s) {
    if (SIDE.equals(s)) {
      return SideAccess.getInstance();
    }
    else if (s != null && s.startsWith(SIDES)) {
      final SequenceEncoder.Decoder sd =
        new SequenceEncoder.Decoder(s.substring(SIDES.length()), ':');
      final ArrayList<String> l = new ArrayList<>();
      while (sd.hasMoreTokens()) {
        l.add(sd.nextToken());
      }
      return new SpecifiedSideAccess(l);
    }
    else {
      return PlayerAccess.getInstance();
    }
  }

  public static String encode(PieceAccess p) {
    String s = null;
    if (p instanceof SideAccess) {
      s = SIDE;
    }
    else if (p instanceof SpecifiedSideAccess) {
      final SequenceEncoder se = new SequenceEncoder(':');
      for (final String side : ((SpecifiedSideAccess)p).getSides()) se.append(side);
      s = se.getValue() == null ? SIDES : SIDES + se.getValue();
    }
    else if (p instanceof PlayerAccess) {
      s = PLAYER;
    }
    return s;
  }
}
