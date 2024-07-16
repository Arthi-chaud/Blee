import * as tanstack from "@tanstack/vue-query";
import { API } from "~/api/api";
import type PaginatedResponse from "~/models/domain/page";
import type {
    PageParameter,
    PaginatedQuery,
    PaginatedQueryOptions,
    Query,
    QueryOptions,
} from "~/models/queries";

const useInfiniteQuery = <ReturnType>(
    query: PaginatedQuery<ReturnType>,
    options?: PaginatedQueryOptions<ReturnType>,
) => {
    return tanstack.useInfiniteQuery<
        PaginatedResponse<ReturnType>,
        Error,
        tanstack.InfiniteData<PaginatedResponse<ReturnType>, PageParameter>,
        tanstack.QueryKey,
        PageParameter
    >({
        ...options,
        queryKey: query.queryKey,
        queryFn: query.queryFn,
        getNextPageParam: (lastPage, _, lastPageParam) => {
            if (lastPage.metadata.next == null) {
                return undefined;
            }
            return {
                skip: lastPageParam.skip + lastPage.items.length,
                take: API.defaultPageSize,
            };
        },
        initialPageParam: {
            skip: 0,
            take: API.defaultPageSize,
        },
    });
};

const useQuery = <ReturnType>(
    query: Query<ReturnType>,
    options?: QueryOptions<ReturnType>,
) => {
    return tanstack.useQuery<ReturnType, Error, ReturnType, tanstack.QueryKey>({
        ...options,
        queryKey: query.queryKey,
        queryFn: query.queryFn,
    });
};

export { useInfiniteQuery, useQuery };
