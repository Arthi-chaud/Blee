/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/no-extraneous-class */
/* eslint-disable @typescript-eslint/ban-types */
import type { RequireExactlyOne } from "type-fest";
import type { Schema } from "yup";
import { Artist } from "~/models/domain/artist";
import { ExternalId } from "~/models/domain/external-id";
import { Extra } from "~/models/domain/extra";
import { Package, type PackageSortingKeys } from "~/models/domain/package";
import PaginatedResponse from "~/models/domain/page";
import type { PageParameter, PaginatedQuery, Query } from "~/models/queries";

type Sort<T extends string> = {
    sort: T;
    order: "asc" | "desc";
};
class API {
    public static readonly defaultPageSize = 25;

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

    static getExternalIds(filter: {
        package?: string;
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
        filter: {},
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
        } & RequireExactlyOne<{
            emptyResponse: true;
            validator: Schema<ReturnType>;
        }>,
    ) {
        const headers = {
            "Content-Type": "application/json",
        };
        const host = `/api`;
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
