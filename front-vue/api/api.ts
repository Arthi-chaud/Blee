import type { QueryFunction, QueryOptions } from "@tanstack/vue-query";
import type { RequireExactlyOne } from "type-fest";
import type { Schema } from "yup";
import { Artist } from "~/models/domain/artist";
import { Package, type PackageSortingKeys } from "~/models/domain/package";
import PaginatedResponse from "~/models/domain/page";
import type { PageParameter, PaginatedQuery, Query } from "~/models/queries";

type Sort<T extends string> = {
    sortBy: T;
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
    static getPackages(
        filter: { artistUuid?: string },
        sort: Sort<PackageSortingKeys>,
    ): PaginatedQuery<Package> {
        const route = `/packages`;
        const params = { ...sort, ...filter };
        return {
            queryKey: this.buildQueryKey(route, params),
            queryFn: ({ pageParam }) =>
                this._fetch(route, {
                    query: { ...params, ...pageParam },
                    validator: PaginatedResponse(Package),
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
            Object.entries(queryParams ?? {}).map(
                ([key, value]) => `${key}=${value}`,
            ),
        ];
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
            customValidator: (value: unknown) => Promise<ReturnType>;
        }>,
    ) {
        const headers = {
            "Content-Type": "application/json",
        };
        const host = isSSR() ? process.env.SSR_SERVER_URL : `/api`;
        const url = new URL(`${host}${route}`);
        Object.entries(options.query ?? {}).forEach(([key, value]) =>
            url.searchParams.append(key, value.toString()),
        );
        // HotFix, as pageParams if null for first page
        if (!url.searchParams.get("take")) {
            url.searchParams.set("take", API.defaultPageSize.toString());
        }
        const response = await $fetch.raw(url.toString(), {
            method: options.method ?? "GET",
            parseResponse: (txt) =>
                options.emptyResponse ? undefined : JSON.parse(txt),
            body: options.body ? JSON.stringify(options.body) : undefined,
            headers,
        });
        const jsonResponse = response._data as Record<string, string>;
        switch (response.status) {
            case 404:
                throw new Error(
                    options.errorMessage ??
                        jsonResponse.message ??
                        response.statusText,
                );
            default:
                if (!response.ok) {
                    throw new Error(
                        options.errorMessage ??
                            jsonResponse.message ??
                            response.statusText,
                    );
                }
        }
        if (options.emptyResponse) {
            return {} as ReturnType;
        }
        try {
            if (options.customValidator) {
                return await options.customValidator(jsonResponse);
            }
            const validated = await options.validator.validate(jsonResponse);

            return options.validator.cast(validated);
        } catch (err) {
            // eslint-disable-next-line no-console
            console.error(jsonResponse, err);
            throw new Error("Error: Invalid Response Type");
        }
    }
}
export { API };
