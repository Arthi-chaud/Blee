import 'package:freezed_annotation/freezed_annotation.dart';

part 'file.freezed.dart';
part 'file.g.dart';

@freezed
class File with _$File {
  const factory File({
    required String id,
    required String path,
    required int size,
    required int duration,
    required VideoQuality? quality,
  }) = _File;

  factory File.fromJson(Map<String, dynamic> json) => _$FileFromJson(json);
}

enum VideoQuality {
  @JsonValue("8k")
  _8k,
  @JsonValue("4k")
  _4k,
  @JsonValue("2k")
  _2k,
  @JsonValue("1080p")
  _1080p,
  @JsonValue("720p")
  _720p,
  @JsonValue("576p")
  _576p,
  @JsonValue("480p")
  _480p,
  @JsonValue("360p")
  _360p,
  @JsonValue("240p")
  _240p,
  @JsonValue("other")
  Other,
}
