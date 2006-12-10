package VASSAL.chat;

/**
 * Immutable PlayerStatus class with flags indicating "looking for a game" and "away from keyboard" and a String profile
 * 
 * @author rkinney
 * 
 */
public class SimpleStatus implements PlayerStatus {
  private boolean looking;
  private boolean away;
  private String profile;

  public boolean isAway() {
    return away;
  }

  public boolean isLooking() {
    return looking;
  }

  public String getProfile() {
    return profile;
  }

  public SimpleStatus(boolean looking, boolean away, String profile) {
    super();
    this.looking = looking;
    this.away = away;
    this.profile = profile;
  }
}
