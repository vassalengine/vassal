/*
 * $Id$
 *
 * Copyright (c) 2000-2007 by Rodney Kinney, Brent Easton
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
package VASSAL.build.module;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.ConfigureTree;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceDefiner;

/**
 * Plugin is a general purpose component for use by Module Plugins that
 * require some sort of initialisation. Module Plugins do not need
 * to include a Plugin component if they consist only of code and
 * image resources used by other parts of the module.
 *
 * Plugin should be subclassed and added to the Module Plugin.
 *
 */
public class Plugin extends AbstractConfigurable implements PluginsLoader.PluginElement {

  /*
   * The module has not been built yet, so only minimal
   * initialisation should be performed in the constructor
   */
  public Plugin() {

  }

  /**
   * init() is called by the GameModule when the build of the module and
   * all extensions is complete. Any initialisation that depends
   * on other parts of the module should be implemented here
   */
  public void init() {

  }

  /**
   * Utility routine to register a GamePiece with the PieceDefiner
   * so that it appears as an option in the list of traits
   * @param p GamePiece to register
   */
  public void registerGamePiece(GamePiece p) {
    PieceDefiner.addDefinition(p);
  }

  /**
   * Utility routine to register a CommandEncoder with the module
   * @param encoder
   */
  public void registerCommandEncoder(CommandEncoder encoder) {
    GameModule.getGameModule().addCommandEncoder(encoder);
  }

  /**
   * Utility routine to register a new component with the module
   * editor so that it appears in the right-click popup menu. To add
   * components to the top module level, use BasicModule.class, not
   * GameModule.class as the parent.
   * @param parent parent of the new component
   * @param child new component
   */
  public void registerComponent(Class<? extends Buildable> parent, Class<? extends Buildable> child) {
    ConfigureTree.addAdditionalComponent(parent, child);
  }

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[0];
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public String getAttributeValueString(String key) {
    return null;
  }

  public void setAttribute(String key, Object value) {

  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public void removeFrom(Buildable parent) {

  }

  public void addTo(Buildable parent) {

  }

}
