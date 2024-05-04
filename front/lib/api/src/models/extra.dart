import 'package:blee/api/src/models/image.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'extra.freezed.dart';
part 'extra.g.dart';

@freezed
class Extra with _$Extra {
  const factory Extra({
    required String id,
    required String name,
    required Image? thumbnail,
    @JsonKey(name: "package_id") required String? packageId,
    @JsonKey(name: "artist_id") required String artistId,
    @JsonKey(name: "artist_name") required String artistName,
    @JsonKey(name: "file_id") required String fileId,
    required int? discIndex,
    required int duration,
    required int? trackIndex,
    required List<ExtraType> type,
  }) = _Extra;

  factory Extra.fromJson(Map<String, dynamic> json) => _$ExtraFromJson(json);
}

enum ExtraType {
  @JsonValue("alternate_view")
  aternateView,
  @JsonValue("backdrops")
  backdrops,
  @JsonValue("behind_the_scenes")
  behindTheScenes,
  @JsonValue("interview")
  interview,
  @JsonValue("music_video")
  musicVideo,
  @JsonValue("other")
  other,
  @JsonValue("performance")
  performance,
  @JsonValue("trailer")
  trailer,
}
