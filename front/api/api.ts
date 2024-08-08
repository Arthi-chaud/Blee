/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/no-extraneous-class */
/* eslint-disable @typescript-eslint/ban-types */
import type { RequireExactlyOne } from "type-fest";
import type { Schema } from "yup";
import { Artist } from "~/models/domain/artist";
import { Chapter } from "~/models/domain/chapter";
import { ExternalId } from "~/models/domain/external-id";
import { Extra } from "~/models/domain/extra";
import { File } from "~/models/domain/file";
import { Movie, type MovieSortingKeys } from "~/models/domain/movie";
import { Package, type PackageSortingKeys } from "~/models/domain/package";
import PaginatedResponse from "~/models/domain/page";
import { ScannerStatusResponse } from "~/models/domain/scanner-status";
import type { PageParameter, PaginatedQuery, Query } from "~/models/queries";
import { Base64 } from "js-base64";
import { FileInfo } from "~/models/domain/file-info";

type Sort<T extends string> = {
    sort: T;
    order: "asc" | "desc";
};
class API {
    public static readonly defaultPageSize = 25;

    static buildImageUrl(imageId: string) {
        return "/api/images/" + imageId;
    }

    private static b64encodeFilePath(filePath: string) {
        return Base64.encodeURI(filePath);
    }

    static buildDirectPlaybackUrl(file: File) {
        return `/transcoder/${this.b64encodeFilePath(file.path)}/direct`;
    }
    static buildTranscodedPlaybackUrl(file: File) {
        return `/transcoder/${this.b64encodeFilePath(file.path)}/master.m3u8`;
    }

    static getArtist(artistUuid: string): Query<Artist> {
        const route = `/artists/${artistUuid}`;
        return {
            queryKey: this.buildQueryKey(route),
            queryFn: () =>
                this._fetch(route, {
                    validator: Artist,
                }),
        };
    }
    static getFile(fileUUID: string): Query<File> {
        const route = `/files/${fileUUID}`;
        return {
            queryKey: this.buildQueryKey(route),
            queryFn: () =>
                this._fetch(route, {
                    validator: File,
                }),
        };
    }
    static getExtra(extraUuid: string): Query<Extra> {
        const route = `/extras/${extraUuid}`;
        return {
            queryKey: this.buildQueryKey(route),
            queryFn: () =>
                this._fetch(route, {
                    validator: Extra,
                }),
        };
    }
    static getMovie(movieUuid: string): Query<Movie> {
        const route = `/movies/${movieUuid}`;
        return {
            queryKey: this.buildQueryKey(route),
            queryFn: () =>
                this._fetch(route, {
                    validator: Movie,
                }),
        };
    }
    static getPackage(packageUuid: string): Query<Package> {
        const route = `/packages/${packageUuid}`;
        return {
            queryKey: this.buildQueryKey(route),
            queryFn: () =>
                this._fetch(route, {
                    validator: Package,
                }),
        };
    }
    static getMovies(
        filter: { package: string },
        sort: Sort<MovieSortingKeys>,
    ): PaginatedQuery<Movie> {
        const route = `/movies`;
        const params = { ...sort, ...filter };
        return {
            queryKey: this.buildQueryKey(route, params),
            queryFn: (pageParam) =>
                this._fetch(route, {
                    query: { ...params, ...pageParam },
                    validator: PaginatedResponse(Movie),
                }),
        };
    }
    static getPackages(
        filter: { artist?: string },
        sort: Sort<PackageSortingKeys>,
    ): PaginatedQuery<Package> {
        const route = `/packages`;
        const params = { ...sort, ...filter };
        return {
            queryKey: this.buildQueryKey(route, params),
            queryFn: (pageParam) =>
                this._fetch(route, {
                    query: { ...params, ...pageParam },
                    validator: PaginatedResponse(Package),
                }),
        };
    }

    static getChapters(movieUuid: string): PaginatedQuery<Chapter> {
        const route = `/movies/${movieUuid}/chapters`;
        return {
            queryKey: this.buildQueryKey(route),
            queryFn: (pageParam) =>
                this._fetch(route, {
                    query: { ...pageParam },
                    validator: PaginatedResponse(Chapter),
                }),
        };
    }

    static getScannerStatus(): Query<ScannerStatusResponse> {
        const route = `/status`;
        return {
            queryKey: this.buildQueryKey(route),
            queryFn: () =>
                this._fetch(route, {
                    host: "scanner",
                    validator: ScannerStatusResponse,
                }),
        };
    }

    static scanNewFiles() {
        return this._fetch("/scan", {
            host: "scanner",
            method: "POST",
            emptyResponse: true,
        });
    }

    static cleanFiles() {
        return this._fetch("/clean", {
            host: "scanner",
            method: "POST",
            emptyResponse: true,
        });
    }

    static getExternalIds(filter: {
        package?: string;
        artist?: string;
    }): PaginatedQuery<ExternalId> {
        const route = `/external_ids`;
        const params = { ...filter };
        return {
            queryKey: this.buildQueryKey(route, params),
            queryFn: (pageParam) =>
                this._fetch(route, {
                    query: { ...params, ...pageParam },
                    validator: PaginatedResponse(ExternalId),
                }),
        };
    }

    static getExtras(
        filter: { artist?: string; package?: string },
        sort: Sort<PackageSortingKeys>,
    ): PaginatedQuery<Extra> {
        const route = `/extras`;
        const params = { ...sort, ...filter };
        return {
            queryKey: this.buildQueryKey(route, params),
            queryFn: (pageParam) =>
                this._fetch(route, {
                    query: { ...params, ...pageParam },
                    validator: PaginatedResponse(Extra),
                }),
        };
    }

    static getFileInfo(file: File): Query<FileInfo> {
        const route = `/${this.b64encodeFilePath(file.path)}/info`;
        return {
            queryKey: this.buildQueryKey(route),
            queryFn: () =>
                this._fetch(route, {
                    host: "transcoder",
                    validator: FileInfo,
                }),
        };
    }

    static getArtists(sort: Sort<PackageSortingKeys>): PaginatedQuery<Artist> {
        const route = `/artists`;
        const params = { ...sort };
        return {
            queryKey: this.buildQueryKey(route, params),
            queryFn: (pageParam) =>
                this._fetch(route, {
                    query: { ...params, ...pageParam },
                    validator: PaginatedResponse(Artist),
                }),
        };
    }

    // Builds a `queryKey` for tanstack's `useQuery`-type functions
    private static buildQueryKey(
        route: string,
        queryParams?: Record<string, string | number>,
    ) {
        return [
            ...route.split("/"),
            ...Object.entries(queryParams ?? {}).map(
                ([key, value]) => `${key}=${value}`,
            ),
        ].filter((v) => v.length > 0);
    }

    private static async _fetch<ReturnType>(
        route: string,
        options: {
            query?: Record<string, string | number>;
            body?: Object;
            method?: "GET" | "POST";
            errorMessage?: string;
            pagination?: PageParameter;
            host?: "api" | "transcoder" | "scanner";
        } & RequireExactlyOne<{
            emptyResponse: true;
            validator: Schema<ReturnType>;
        }>,
    ) {
        const headers = {
            "Content-Type": "application/json",
        };
        const host =
            options.host === "transcoder"
                ? `/transcoder`
                : options.host === "scanner"
                  ? "scanner"
                  : `/api`;
        let url = `${host}${route}`;
        if (options.query) {
            url = url.concat("?");
        }
        Object.entries(options.query ?? {}).forEach(
            ([key, value]) => (url = url.concat(`${key}=${value}&`)),
        );
        return $fetch(url, {
            method: options.method ?? "GET",
            parseResponse: (txt) =>
                options.emptyResponse ? txt : JSON.parse(txt),
            onResponseError: (e) => {
                const jsonResponse = e.response as Record<string, any>;
                switch (e.response.status) {
                    case 404:
                        throw new Error(
                            options.errorMessage ??
                                jsonResponse.message ??
                                e.response.statusText,
                        );
                    default:
                        if (!jsonResponse.ok) {
                            throw new Error(
                                options.errorMessage ??
                                    jsonResponse.message ??
                                    e.response.statusText,
                            );
                        }
                }
            },
            body: options.body ? JSON.stringify(options.body) : undefined,
            headers,
        }).then((response) => {
            const jsonResponse = response as Record<string, any>;

            if (options.emptyResponse) {
                return {} as ReturnType;
            }
            try {
                const validated = options.validator.validateSync(jsonResponse);
                return options.validator.cast(validated);
            } catch (err) {
                console.error(jsonResponse, err);
                throw new Error("Error: Invalid Response Type");
            }
        });
    }
}
export { API };
