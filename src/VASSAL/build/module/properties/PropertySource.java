package VASSAL.build.module.properties;

public interface PropertySource {
  Object getProperty(Object key);
  Object getLocalizedProperty(Object key);
}
