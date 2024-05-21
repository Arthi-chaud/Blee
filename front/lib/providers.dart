import 'package:blee/api/api.dart';
import 'package:blee/api/src/client.dart' as api;
import 'package:blee/api/src/models/order.dart';
import 'package:blee/api/src/models/scanner.dart';
import 'package:blee/models/models.dart';
import 'package:riverpod_annotation/riverpod_annotation.dart';
part 'providers.g.dart';

@riverpod
class ApiClient extends _$ApiClient {
  @override
  api.APIClient build() {
    return api.APIClient();
  }
}

@riverpod
Future<Package> getPackage(GetPackageRef ref, String packageUuid) async {
  APIClient client = ref.watch(apiClientProvider);
  return await client.getPackage(packageUuid);
}

@riverpod
Future<Artist> getArtist(GetArtistRef ref, String artistUuid) async {
  APIClient client = ref.watch(apiClientProvider);
  return await client.getArtist(artistUuid);
}

@riverpod
Future<File> getFile(GetFileRef ref, String fileUuid) async {
  APIClient client = ref.watch(apiClientProvider);
  return await client.getFile(fileUuid);
}

@riverpod
Future<Extra> getExtra(GetExtraRef ref, String extraUuid) async {
  APIClient client = ref.watch(apiClientProvider);
  return await client.getExtra(extraUuid);
}

@riverpod
Future<Movie> getMovie(GetMovieRef ref, String movieUuid) async {
  APIClient client = ref.watch(apiClientProvider);
  return await client.getMovie(movieUuid);
}

@riverpod
Future<Page<Movie>> getMovies(
  GetMoviesRef ref, {
  String? packageUuid,
  MovieSort sort = MovieSort.name,
  Ordering order = Ordering.asc,
}) async {
  APIClient client = ref.watch(apiClientProvider);
  return await client.getMovies(
      packageUuid: packageUuid, sort: sort, order: order);
}

@riverpod
Future<Page<Chapter>> getChapters(
    GetChaptersRef ref, String movieUuid, PageQuery page) async {
  APIClient client = ref.watch(apiClientProvider);
  return await client.getChapters(movieUuid, page);
}

@riverpod
Future<Page<Extra>> getExtras(
  GetExtrasRef ref, {
  String? packageUuid,
  PageQuery page = const PageQuery(),
  ExtraSort sort = ExtraSort.name,
  Ordering order = Ordering.asc,
}) async {
  APIClient client = ref.watch(apiClientProvider);
  return await client.getExtras(
      packageUuid: packageUuid, page: page, sort: sort, order: order);
}

@riverpod
Future<Page<Artist>> getArtists(GetArtistsRef ref,
    {String? packageUuid,
    ArtistSort sort = ArtistSort.name,
    Ordering order = Ordering.asc,
    PageQuery page = const PageQuery()}) async {
  APIClient client = ref.watch(apiClientProvider);
  return await client.getArtists(
      page: page, package: packageUuid, sort: sort, order: order);
}

@riverpod
Future<Page<ExternalId>> getPackageExternalIds(
    GetPackageExternalIdsRef ref, String packageUuid) async {
  APIClient client = ref.watch(apiClientProvider);
  return await client.getPackageExternalIds(packageUuid);
}

@riverpod
Future<Page<ExternalId>> getArtistExternalIds(
    GetArtistExternalIdsRef ref, String artistUuid) async {
  APIClient client = ref.watch(apiClientProvider);
  return await client.getArtistExternalIds(artistUuid);
}

//// Scanner
@riverpod
Future<ScannerStatusResponse> getScannerStatus(GetScannerStatusRef ref) async {
  APIClient client = ref.watch(apiClientProvider);
  return await client.getScannerStatus();
}

//// Player

@riverpod
Future<PlayerMetadata> getPlayerMetadataFromExtraUuid(
    GetPlayerMetadataFromExtraUuidRef ref, String extraUuid) async {
  final extra = await ref.watch(getExtraProvider(extraUuid).future);
  final file = await ref.watch(getFileProvider(extra.fileId).future);
  final package = await ref.watch(getPackageProvider(extra.packageId).future);

  return PlayerMetadata(
      videoTitle: extra.name,
      parentUrlResource: '/packages/${extra.packageId}',
      videoArtist: extra.artistName,
      poster: package.poster,
      thumbnail: extra.thumbnail,
      chapters: [],
      videoFile: file);
}

@riverpod
Future<PlayerMetadata> getPlayerMetadataFromMovieUuid(
    GetPlayerMetadataFromMovieUuidRef ref, String movieUuid) async {
  final movie = await ref.watch(getMovieProvider(movieUuid).future);
  final file = await ref.watch(getFileProvider(movie.fileId).future);
  final package = await ref.watch(getPackageProvider(movie.packageId).future);
  final chapters = await ref.watch(
      getChaptersProvider(movie.id, const PageQuery(take: 100)).future); // TODO

  return PlayerMetadata(
      videoTitle: movie.name,
      parentUrlResource: '/packages/${movie.packageId}',
      videoArtist: movie.artistName,
      poster: package.poster,
      thumbnail: movie.thumbnail,
      chapters: chapters.items,
      videoFile: file);
}
