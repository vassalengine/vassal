/*
 *
 * Copyright (c) 2021 by The VASSAL Development Team
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
package VASSAL.build.module.map.deck;

import VASSAL.build.AutoConfigurable;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.i18n.Resources;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;

public class DeckSortKeyCommand extends AbstractDeckKeyCommand {

  public static final String SORT_PARAMETERS = "sortParameters";

  private List<SortParameter> sortParameters;

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.DeckSortKeyCommand.component_type"); //$NON-NLS-1$
  }

  public List<SortParameter> getSortParameters() {
    if (sortParameters == null) {
      // Start with a default entry
      sortParameters = new ArrayList<>();
      sortParameters.add(new SortParameter(true, "Property"));
    }
    return sortParameters;
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(super.getAttributeNames(), SORT_PARAMETERS);
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.addAll(super.getAttributeDescriptions(), Resources.getString("Editor.DeckSortKeyCommand.sort_by"));
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.addAll(super.getAttributeTypes(), SortParameterConfig.class);
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (SORT_PARAMETERS.equals(key)) {
      if (value instanceof String) {
        value = SortParameterArrayConfigurer.decode((String) value);
      }
      sortParameters = (List<SortParameter>) value;
    }
    else {
      super.setAttribute(key, value);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (SORT_PARAMETERS.equals(key)) {
      return SortParameterArrayConfigurer.encode(sortParameters);
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  public static class SortParameterConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new SortParameterArrayConfigurer(key, name, ((DeckSortKeyCommand) c).getSortParameters());
    }
  }
}




