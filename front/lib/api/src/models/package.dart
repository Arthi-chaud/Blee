import 'package:blee/api/src/models/image.dart';
import 'package:change_case/change_case.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'package.freezed.dart';
part 'package.g.dart';

@freezed
class Package with _$Package {
  const factory Package(
      {required String id,
      required String name,
      required String slug,
      @JsonKey(name: "release_year") required DateTime? releaseDate,
      @JsonKey(name: "artist_id") required String? artistId,
      @JsonKey(name: "artist_name") required String? artistName,
      required Image? poster}) = _Package;

  factory Package.fromJson(Map<String, dynamic> json) =>
      _$PackageFromJson(json);
}

enum PackageSort {
  name,
  addDate,
  artistName,
  releaseDate;

  @override
  String toString() {
    return this.name.toSnakeCase();
  }
}
