package VASSAL.build.module.properties;

/**
 * Provides a new value for a global property. This class is an abstraction around the act of prompting a user for the
 * new value of a property during a game. Concrete implementation might be to increment a value, prompt the user to
 * select from an enum, etc.
 * 
 * @author rkinney
 * 
 */
public interface PropertyChanger {
  String getNewValue(String oldValue);
}
