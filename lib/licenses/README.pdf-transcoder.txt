The pdf-transcoder.jar file from the Apache Fop project
(http://xml.apache.org/fop), this is only needed if you want to
transcode to PDF, otherwise it can be removed from distributions.

This jar file is slightly modified from the standard
pdf-transcoder.jar to also includes two required classes from the
Apache Avalon project (http://avalon.apache.org):
org.apache.avalon.framework.Enum and
org.apache.avalon.framework.ValuedEnum.
