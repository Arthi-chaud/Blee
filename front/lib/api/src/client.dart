import 'dart:convert';

import 'package:blee/api/api.dart';
import 'package:flutter/foundation.dart';
import 'package:http/http.dart' as http;

enum RequestType { get, post, put, delete }

class APIClient {
  String _host = Uri.base.toString();

  final http.Client client = http.Client();

  APIClient() {
    if (kDebugMode) {
      const url = String.fromEnvironment("API_URL");
      _host = url;
    }
  }

  Future<Artist> getArtist(String uuid) async {
    var responseBody = await _request(RequestType.get, '/artists/$uuid');
    return Artist.fromJson(responseBody);
  }

  Future<Package> getPackage(String uuid) async {
    var responseBody = await _request(RequestType.get, '/packages/$uuid');
    return Package.fromJson(responseBody);
  }

  Future<Map<String, dynamic>> _request(RequestType type, String route,
      {Map<String, dynamic>? body, Map<String, dynamic>? params}) async {
    body ??= {};
    params ?? {};
    http.Response response;
    Uri fullRoute = Uri.parse(_host +
        (kDebugMode ? route : "api$route") +
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
