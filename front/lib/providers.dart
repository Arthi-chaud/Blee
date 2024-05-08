import 'package:blee/api/api.dart';
import 'package:blee/models/models.dart';
import 'package:riverpod_annotation/riverpod_annotation.dart';
part 'providers.g.dart';

@riverpod
Future<Package> getPackage(GetPackageRef ref, String packageUuid) async {
  return await APIClient().getPackage(packageUuid);
}

@riverpod
Future<File> getFile(GetFileRef ref, String fileUuid) async {
  return await APIClient().getFile(fileUuid);
}

@riverpod
Future<Extra> getExtra(GetExtraRef ref, String extraUuid) async {
  return await APIClient().getExtra(extraUuid);
}

@riverpod
Future<Movie> getMovie(GetMovieRef ref, String movieUuid) async {
  return await APIClient().getMovie(movieUuid);
}

@riverpod
Future<Page<Movie>> getMovies(GetMoviesRef ref, {String? packageUuid}) async {
  return await APIClient().getMovies(packageUuid!);
}

@riverpod
Future<Page<Chapter>> getChapters(
    GetChaptersRef ref, String movieUuid, PageQuery page) async {
  return await APIClient().getChapters(movieUuid, page);
}

@riverpod
Future<Page<Extra>> getExtras(GetExtrasRef ref,
    {String? packageUuid, PageQuery page = const PageQuery()}) async {
  return await APIClient().getExtras(packageUuid: packageUuid, page: page);
}

@riverpod
Future<Page<ExternalId>> getPackageExternalIds(
    GetPackageExternalIdsRef ref, String packageUuid) async {
  return await APIClient().getPackageExternalIds(packageUuid);
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
