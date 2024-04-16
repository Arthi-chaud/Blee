import 'package:blee/api/src/models/image.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'movie.freezed.dart';
part 'movie.g.dart';

@freezed
class Movie with _$Movie {
  const factory Movie({
    required String id,
    required String name,
    required String slug,
    required Image? thumbnail,
    @JsonKey(name: "package_id") required String packageId,
    @JsonKey(name: "artist_id") required String artistId,
    @JsonKey(name: "file_id") required String fileId,
    required MovieType type,
  }) = _Movie;

  factory Movie.fromJson(Map<String, dynamic> json) => _$MovieFromJson(json);
}

enum MovieType {
  @JsonValue("documentary")
  Documentary,
  @JsonValue("concert")
  Concert,
}
