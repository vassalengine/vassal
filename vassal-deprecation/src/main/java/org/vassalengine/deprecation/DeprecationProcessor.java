package org.vassalengine.deprecation;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.annotation.processing.SupportedSourceVersion;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;
import java.time.LocalDate;
import java.time.Period;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@SupportedAnnotationTypes(value = "java.lang.Deprecated")
@SupportedSourceVersion(SourceVersion.RELEASE_11)
public final class DeprecationProcessor extends AbstractProcessor {

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    for (TypeElement annotationType : annotations) {
      final Set<? extends Element> elements = roundEnv.getElementsAnnotatedWith(annotationType);

      final Map<Boolean, List<Element>> partitionByValidity = elements.stream().collect(
        Collectors.partitioningBy(element -> isValidDate(element.getAnnotation(Deprecated.class).since())));

      partitionByValidity
        .get(Boolean.TRUE)
        .stream()
        .filter(element -> hasYearPassedSinceDeprecation(element.getAnnotation(Deprecated.class).since()))
        .forEach(element ->
          processingEnv.getMessager().printMessage(
            Diagnostic.Kind.WARNING,
            buildMessage(element, "Element has been deprecated for over a year and is due for removal")));
    }

    // do not return true here else the annotation will be consumed / hidden from further processing
    return false;
  }

  private static String buildMessage(Element element, String message) {
    final StringBuilder sb = new StringBuilder();

    if (!element.getKind().isClass()) {
      sb
        .append(element.getEnclosingElement())
        .append(".");
    }

    sb
      .append(element.toString())
      .append(": ")
      .append(message);

    return sb.toString();
  }

  private static boolean isValidDate(String input) {
    if (input == null || input.trim().isEmpty()) {
      return false;
    }

    try {
      DateTimeFormatter.ISO_LOCAL_DATE.parse(input);
    }
    catch (DateTimeParseException e) {
      return false;
    }

    return true;
  }

  private static boolean hasYearPassedSinceDeprecation(String input) {
    final LocalDate deprecationDate = LocalDate.from(DateTimeFormatter.ISO_LOCAL_DATE.parse(input));
    return Period.between(deprecationDate, LocalDate.now()).getYears() >= 1;
  }
}
