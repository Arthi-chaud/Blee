import 'dart:convert';

import 'package:blee/api/api.dart';
import 'package:blee/api/src/models/order.dart';
import 'package:flutter/foundation.dart';
import 'package:http/http.dart' as http;

enum RequestType { get, post, put, delete }

class APIClient {
  late String _host;

  final http.Client client = http.Client();

  APIClient() {
    _host = Uri.base.toString();
    var poundPos = Uri.base.toString().indexOf('/#/');
    if (poundPos != -1) {
      _host = Uri.base.toString().substring(0, poundPos);
    }
    if (kDebugMode) {
      const url = String.fromEnvironment("API_URL");
      _host = url;
    }
  }

  String buildImageUrl(String uuid) {
    final route = "/images/$uuid";
    return _host + (kDebugMode ? route : "/api$route");
  }

  String buildTranscoderUrl(String transcoderRoute) {
    if (kDebugMode) {
      return 'http://0.0.0.0:7666$transcoderRoute';
    }
    return "$_host/transcoder$transcoderRoute";
  }

  Future<Artist> getArtist(String uuid) async {
    var responseBody = await _request(RequestType.get, '/artists/$uuid');
    return Artist.fromJson(responseBody);
  }

  Future<Package> getPackage(String uuid) async {
    var responseBody = await _request(RequestType.get, '/packages/$uuid');
    return Package.fromJson(responseBody);
  }

  Future<File> getFile(String uuid) async {
    var responseBody = await _request(RequestType.get, '/files/$uuid');
    return File.fromJson(responseBody);
  }

  Future<Extra> getExtra(String uuid) async {
    var responseBody = await _request(RequestType.get, '/extras/$uuid');
    return Extra.fromJson(responseBody);
  }

  Future<Movie> getMovie(String uuid) async {
    var responseBody = await _request(RequestType.get, '/movies/$uuid');
    return Movie.fromJson(responseBody);
  }

  Future<Page<Package>> getPackages(
      {PageQuery page = const PageQuery(), String? artistUuid}) async {
    var responseBody = await _request(RequestType.get,
        '/packages?artist=${artistUuid ?? ''}&take=${page.take}&skip=${page.skip}');
    return Page.fromJson(
        responseBody, (x) => Package.fromJson(x as Map<String, dynamic>));
  }

  Future<Page<Artist>> getArtists(
      {PageQuery page = const PageQuery(),
      ArtistSort sort = ArtistSort.name,
      Ordering order = Ordering.asc,
      String? package}) async {
    var responseBody = await _request(RequestType.get,
        '/artists?package=${package ?? ''}&take=${page.take}&skip=${page.skip}&sort=${sort.toString()}&order=${order.toString()}');
    return Page.fromJson(
        responseBody, (x) => Artist.fromJson(x as Map<String, dynamic>));
  }

  Future<Page<ExternalId>> getPackageExternalIds(String packageUuid,
      {PageQuery page = const PageQuery()}) async {
    var responseBody = await _request(RequestType.get,
        '/external_ids?package=$packageUuid&take=${page.take}&skip=${page.skip}');
    return Page.fromJson(
        responseBody, (x) => ExternalId.fromJson(x as Map<String, dynamic>));
  }

  Future<Page<ExternalId>> getArtistExternalIds(String artistUuid,
      {PageQuery page = const PageQuery()}) async {
    var responseBody = await _request(RequestType.get,
        '/external_ids?artist=$artistUuid&take=${page.take}&skip=${page.skip}');
    return Page.fromJson(
        responseBody, (x) => ExternalId.fromJson(x as Map<String, dynamic>));
  }

  Future<Page<Movie>> getMovies(String packageUuid) async {
    var responseBody =
        await _request(RequestType.get, '/movies?package=$packageUuid');
    return Page.fromJson(
        responseBody, (x) => Movie.fromJson(x as Map<String, dynamic>));
  }

  Future<Page<Chapter>> getChapters(String movieUuid, PageQuery query) async {
    var responseBody = await _request(RequestType.get,
        '/movies/$movieUuid/chapters?take=${query.take}&skip=${query.skip}');
    return Page.fromJson(
        responseBody, (x) => Chapter.fromJson(x as Map<String, dynamic>));
  }

  Future<Page<Extra>> getExtras(
      {String? packageUuid,
      String? artistUuid,
      ExtraSort sort = ExtraSort.name,
      Ordering order = Ordering.asc,
      PageQuery page = const PageQuery()}) async {
    var responseBody = await _request(RequestType.get,
        '/extras?package=$packageUuid&artist=$artistUuid&take=${page.take}&skip=${page.skip}&sort=${sort.toString()}&order=${order.toString()}');
    return Page.fromJson(
        responseBody, (x) => Extra.fromJson(x as Map<String, dynamic>));
  }

  Future<dynamic> _request(RequestType type, String route,
      {Map<String, dynamic>? body, Map<String, dynamic>? params}) async {
    body ??= {};
    params ?? {};
    http.Response response;
    Uri fullRoute = Uri.parse(_host +
        (kDebugMode ? route : "/api$route") +
        (params == null ? "" : "?${Uri(queryParameters: params).query}"));
    final Map<String, String> headers = {
      'Content-type': 'application/json',
      'Accept': 'application/json',
    };
    switch (type) {
      case RequestType.get:
        response = await client.get(fullRoute, headers: headers);
        break;
      case RequestType.post:
        response = await client.post(fullRoute, body: body, headers: headers);
        break;
      case RequestType.put:
        response = await client.put(fullRoute, body: body, headers: headers);
        break;
      case RequestType.delete:
        response = await client.delete(fullRoute, body: body, headers: headers);
        break;
    }
    return jsonDecode(response.body);
  }
}
