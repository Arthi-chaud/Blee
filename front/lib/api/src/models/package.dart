import 'package:blee/api/src/models/image.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'package.freezed.dart';
part 'package.g.dart';

@freezed
class Package with _$Package {
  const factory Package({
    required String id,
    required String name,
    required String slug,
    @JsonKey(name: "release_year")
    required DateTime? releaseDate,
    @JsonKey(name: "artist_id")
    required String? artistId,
    required Image? poster
  }) = _Package;

  factory Package.fromJson(Map<String, dynamic> json) => _$PackageFromJson(json);
}