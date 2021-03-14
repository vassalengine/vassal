/*
 *
 * Copyright (c) 2000-2012 by Rodney Kinney, Brent Easton
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
package VASSAL.build;

import VASSAL.search.AbstractImageFinder;
import VASSAL.search.ImageSearchTarget;
import VASSAL.tools.ProblemDialog;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;

import VASSAL.build.module.properties.PropertyNameSource;
import VASSAL.configure.ValidationReport;
import VASSAL.configure.ValidityChecker;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Translatable;

/**
 * Abstract implementation of the {@link Buildable} interface. To make a Buildable component, in other words a component
 * which can be read from the XML buildFile along with a set of attributes, extend this class -- or more likely
 * {@link AbstractConfigurable} if the component is also to be editable/configurable in the Editor window.
 * You'll need to implement the methods and specify the Buildable attributes of this class, and the build process is
 * handled automatically.
 */
public abstract class AbstractBuildable extends AbstractImageFinder implements Buildable, ValidityChecker, PropertyNameSource {
  protected List<Buildable> buildComponents = new ArrayList<>();

  // Sub-classes can set this reference to perform validity checking
  protected ValidityChecker validator;

  /**
   * Build this component by getting all XML attributes of the XML element and
   * calling {@link #setAttribute} with the String value of the attribute
   */
  @Override
  public void build(Element e) {
    if (e == null) {
      return;
    }

    final NamedNodeMap n = e.getAttributes();
    for (int i = 0; i < n.getLength(); ++i) {
      final Attr att = (Attr) n.item(i);
      setAttribute(att.getName(), att.getValue().intern());

      /*
       * Save a record of all Attributes for later translation. Need to save
       * all attributes, not just translatable ones as the current component
       * has not been completely built yet and a ComponentI18nData object
       * cannot be built.
       */
      if (this instanceof Translatable) {
        Localization.getInstance().saveTranslatableAttribute((Translatable) this, att.getName(), att.getValue());
      }
    }
    Builder.build(e, this);
  }

  /**
   * Lists all the buildFile (XML) attribute names for this component.
   * If this component is ALSO an {@link AbstractConfigurable}, then this list of attributes determines the appropriate
   * attribute order for {@link AbstractConfigurable#getAttributeDescriptions()} and {@link AbstractConfigurable#getAttributeTypes()}.
   * @return a list of all buildFile (XML) attribute names for this component
   */
  public abstract String[] getAttributeNames();

  /**
   * Sets a buildFile (XML) attribute value for this component. The <code>key</code> parameter will be one of those listed in {@link #getAttributeNames}.
   * If the <code>value</code> parameter is a String, it will be the value returned by {@link #getAttributeValueString} for the same
   * <code>key</code>. If the implementing class extends {@link AbstractConfigurable}, then <code>value</code> will be an instance of
   * the corresponding Class listed in {@link AbstractConfigurable#getAttributeTypes}
   *
   * @param key the name of the attribute. Will be one of those listed in {@link #getAttributeNames}
   * @param value If the <code>value</code> parameter is a String, it will be the value returned by {@link #getAttributeValueString} for the same
   *              <code>key</code>. If the implementing class extends {@link AbstractConfigurable}, then <code>value</code> can also be an instance of
   *              the corresponding Class listed in {@link AbstractConfigurable#getAttributeTypes}
   */
  public abstract void setAttribute(String key, Object value);

  /**
   * @return a String representation of the XML buildFile attribute with the given name. When initializing a module,
   * this String value will loaded from the XML and passed to {@link #setAttribute}. It is also frequently used for
   * checking the current value of an attribute.
   *
   * @param key the name of the attribute. Will be one of those listed in {@link #getAttributeNames}
   */
  public abstract String getAttributeValueString(String key);

  /**
   * @return all build components that are an instance of the given class
   * @deprecated Use {@link #getComponentsOf(Class)} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public <T> Enumeration<T> getComponents(Class<T> target) {
    ProblemDialog.showDeprecated("2020-08-06");
    return Collections.enumeration(getComponentsOf(target));
  }

  /**
   * @return all build components that are an instance of the given class
   */
  public <T> List<T> getComponentsOf(Class<T> target) {
    final ArrayList<T> l = new ArrayList<>();
    for (final Buildable b : buildComponents) {
      if (target.isInstance(b)) {
        l.add(target.cast(b));
      }
    }
    return l;
  }

  /**
   * Recursively descend the build tree and return an enumeration of all
   * components that are instances of the given class
   *
   * @param target Target class
   * @return Results
   * @deprecated Use {@link #getAllDescendantComponentsOf(Class)} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public <T> Enumeration<T> getAllDescendantComponents(Class<T> target) {
    ProblemDialog.showDeprecated("2020-08-06");
    return Collections.enumeration(getAllDescendantComponentsOf(target));
  }

  /**
   * Recursively descend the build tree and return a {@link List} of all
   * components that are instances of the given class
   *
   * @param target Target class
   * @return {@link List} of all components that are instances of the given class
   */
  public <T> List<T> getAllDescendantComponentsOf(Class<T> target) {
    final ArrayList<T> l = new ArrayList<>();
    addComponents(target, l);
    return l;
  }

  private <T> void addComponents(Class<T> target, List<T> l) {
    if (target.isInstance(this)) {
      l.add(target.cast(this));
    }
    for (final Buildable b : buildComponents) {
      if (b instanceof AbstractBuildable) {
        ((AbstractBuildable) b).addComponents(target, l);
      }
    }
  }

  @Override
  public Element getBuildElement(org.w3c.dom.Document doc) {
    final Element el = doc.createElement(getClass().getName());
    final String[] names = getAttributeNames();
    for (final String name : names) {
      final String val = getAttributeValueString(name);
      if (val != null) {
        el.setAttribute(name, val);
      }
    }

    for (final Buildable b : getBuildables()) {
      el.appendChild(b.getBuildElement(doc));
    }
    return el;
  }

  /**
   * Add a Buildable object to this object
   */
  @Override
  public void add(Buildable b) {
    buildComponents.add(b);
  }

  /**
   * @return an enumeration of Buildable objects which are the direct children
   * of this object in the Buildable containment hierarchy. The
   * {@link #getBuildElement} method uses these objects to construct the XML
   * element from which this object can be built.
   *
   * @deprecated Use {@link #getBuildables()} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Enumeration<Buildable> getBuildComponents() {
    ProblemDialog.showDeprecated("2020-08-06");
    return Collections.enumeration(buildComponents);
  }

  /**
   * @return a Collection of Buildable objects which are the direct children
   * of this object in the Buildable containment hierarchy. The
   * {@link #getBuildElement} method uses these objects to construct the XML
   * element from which this object can be built.
   */
  public List<Buildable> getBuildables() {
    return Collections.unmodifiableList(buildComponents);
  }

  @Override
  public void validate(Buildable target, ValidationReport report) {
    if (validator != null) {
      validator.validate(target, report);
    }
    for (final Buildable child : buildComponents) {
      if (child instanceof ValidityChecker) {
        ((ValidityChecker) child).validate(child, report);
      }
    }
  }

  /**
   * Override this method to provide a list of properties to be exposed for use by expressions in the module.
   * @return Default implementation of PropertyNameSource - No properties exposed
   */
  @Override
  public List<String> getPropertyNames() {
    return new ArrayList<>();
  }

  /**
   * Adds all images used by this component AND any subcomponents to the collection
   * @param s Collection to add image names to
   */
  @Override
  public void addImageNamesRecursively(Collection<String> s) {
    addLocalImageNames(s);

    for (final Buildable child : buildComponents) {
      if (child instanceof ImageSearchTarget) {
        ((ImageSearchTarget) child).addImageNamesRecursively(s);
      }
    }
  }
}
