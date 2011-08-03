/*
 * $Id$
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

import java.awt.Component;
import java.awt.Window;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import VASSAL.counters.PieceAccess;
import VASSAL.counters.PlayerAccess;
import VASSAL.counters.SideAccess;
import VASSAL.counters.SpecifiedSideAccess;
import VASSAL.tools.SequenceEncoder;

public class PieceAccessConfigurer extends Configurer {
  protected static String PLAYER = "player:";
  protected static String SIDE = "side:";
  protected static String SIDES = "sides:";
  protected JPanel controls;
  protected String[] prompts = new String[] {"Any player", "Any side", "Any of the specified sides"};
  protected JComboBox selectType;
  protected StringArrayConfigurer sideConfig;

  public PieceAccessConfigurer(String key, String name, PieceAccess value) {
    super(key, name, value);
  }

  public void setValue(Object o) {
    super.setValue(o);
    updateControls();
  }



  public String getValueString() {
    return encode(getPieceAccess());
  }

  public void setValue(String s) {
    setValue(decode(s));
  }

  public Component getControls() {
    if (controls == null) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));
      Box box = Box.createHorizontalBox();
      box.add(new JLabel(getName()));
      selectType = new JComboBox(getPrompts());
      selectType.addItemListener(new ItemListener() {
        public void itemStateChanged(ItemEvent e) {
          updateValue();
          sideConfig.getControls().setVisible(getValue() instanceof SpecifiedSideAccess);
          if (controls.getTopLevelAncestor() instanceof Window) {
            ((Window)controls.getTopLevelAncestor()).pack();
          }
        }
      });
      box.add(selectType);
      controls.add(box);
      sideConfig = new StringArrayConfigurer(null, null);
      sideConfig.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          updateValue();
        }
      });
      controls.add(sideConfig.getControls());
      updateControls();
    }
    return controls;
  }

  protected void updateValue() {
    noUpdate = true;
    if (prompts[1].equals(selectType.getSelectedItem())) {
      setValue(SideAccess.getInstance());
    }
    else if (prompts[2].equals(selectType.getSelectedItem())) {
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
        selectType.setSelectedIndex(1);
      }
      else if (getValue() instanceof SpecifiedSideAccess) {
        sideConfig.setValue(((SpecifiedSideAccess) getPieceAccess()).getSides().toArray(new String[0]));
        selectType.setSelectedIndex(2);
      }
      else {
        selectType.setSelectedIndex(0);
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
      SequenceEncoder.Decoder sd =
        new SequenceEncoder.Decoder(s.substring(SIDES.length()), ':');
      ArrayList<String> l = new ArrayList<String>();
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
      SequenceEncoder se = new SequenceEncoder(':');
      for (String side : ((SpecifiedSideAccess)p).getSides()) se.append(side);
      s = se.getValue() == null ? SIDES : SIDES + se.getValue();
    }
    else if (p instanceof PlayerAccess) {
      s = PLAYER;
    }
    return s;
  }

  public void setPrompts(String[] prompts) {
    this.prompts = prompts;
    if (selectType == null) {
      selectType.setModel(new DefaultComboBoxModel(prompts));
    }
  }
}
