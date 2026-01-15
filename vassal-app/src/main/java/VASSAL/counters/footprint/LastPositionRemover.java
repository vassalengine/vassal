package VASSAL.counters.footprint;

import VASSAL.build.module.Map;
import org.apache.commons.lang3.Strings;

import java.awt.Point;
import java.util.List;

public class LastPositionRemover {

  private LastPositionRemover() {
  }

  public static void removeLastPositionIfLocationNameNotChanged(
          final Point actualPoint, final Object dropTarget,
          final List<Point> pointList, final Map map) {
    if (dropTarget instanceof Point && map != null
        && pointList != null && !pointList.isEmpty()
        && Strings.CS.equals(map.locationName((Point) dropTarget), map.locationName(actualPoint))) {
      return;
    }
    boolean addPoint = true;
    if (pointList != null && !pointList.isEmpty()) {
      final Point previousPoint = pointList.get(pointList.size() - 1);

      if (previousPoint != null && map != null
          && Strings.CS.equals(map.locationName(previousPoint), map.locationName(actualPoint))
      ) {
        if (pointList.size() != 1) {
          pointList.remove(previousPoint);
        }
        else {
          addPoint = false;
        }
      }
    }
    if (addPoint && pointList != null) {
      pointList.add(actualPoint);
    }
  }
}
