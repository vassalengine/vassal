package VASSAL.chat.ui;


/**
 * Interface for registering event listeners with the Swing components in a ChatServerControls component 
 * @author rkinney
 *
 */
public interface ChatControlsInitializer {
  /** Register all event listeners */
  void initializeControls(ChatServerControls controls);
  /** Remove all previously-registered event listeners */
  void uninitializeControls(ChatServerControls controls);
}
