import 'package:freezed_annotation/freezed_annotation.dart';

part 'external_id.freezed.dart';
part 'external_id.g.dart';

@freezed
class ExternalId with _$ExternalId {
  const factory ExternalId({
    required String url,
    required String value,
    required String? description,
    required int? rating,
    @JsonKey(name: "provider_name")
    required String providerName,
  }) = _ExternalId;

  factory ExternalId.fromJson(Map<String, dynamic> json) => _$ExternalIdFromJson(json);
}