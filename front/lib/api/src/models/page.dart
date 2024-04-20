import 'package:freezed_annotation/freezed_annotation.dart';

part 'page.freezed.dart';
part 'page.g.dart';

@freezed
class PageMetadata with _$PageMetadata {
  const factory PageMetadata({
    required String? next,
    required int count,
  }) = _PageMetadata;

  factory PageMetadata.fromJson(Map<String, dynamic> json) =>
      _$PageMetadataFromJson(json);
}

@Freezed(genericArgumentFactories: true)
class Page<T> with _$Page<T> {
  const factory Page({
    required List<T> items,
    required PageMetadata metadata,
  }) = _Page;

  factory Page.fromJson(
          Map<String, dynamic> json, T Function(Object? json) fromJsonT) =>
      _$PageFromJson(json, fromJsonT);
}
