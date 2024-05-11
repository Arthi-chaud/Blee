import 'package:blee/api/src/models/image.dart';
import 'package:change_case/change_case.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'artist.freezed.dart';
part 'artist.g.dart';

@freezed
class Artist with _$Artist {
  const factory Artist({
    required String id,
    required String name,
    required String slug,
    required Image? poster,
  }) = _Artist;

  factory Artist.fromJson(Map<String, dynamic> json) => _$ArtistFromJson(json);
}

enum ArtistSort {
  name,
  addDate;

  @override
  String toString() {
    return this.name.toSnakeCase();
  }
}
