import type * as tanstack from "@tanstack/vue-query";
import type PaginatedResponse from "./domain/page";

type PageParameter = { take: number; skip: number };

type Query<ReturnType> = {
    queryKey: tanstack.QueryKey;
    queryFn: tanstack.QueryFunction<ReturnType, tanstack.QueryKey>;
};

type QueryOptions<ReturnType> = Omit<
    tanstack.UseQueryOptions<ReturnType, Error, ReturnType, tanstack.QueryKey>,
    "queryFn" | "queryKey" | "initialData"
>;

type PaginatedQuery<ReturnType> = {
    queryKey: tanstack.QueryKey;
    queryFn: (page: PageParameter) => Promise<PaginatedResponse<ReturnType>>;
};

type PaginatedQueryOptions<ReturnType> = Omit<
    tanstack.UseInfiniteQueryOptions<
        PaginatedResponse<ReturnType>,
        Error,
        tanstack.InfiniteData<PaginatedResponse<ReturnType>, PageParameter>,
        PaginatedResponse<ReturnType>,
        tanstack.QueryKey,
        PageParameter
    >,
    | "queryFn"
    | "queryKey"
    | "initialData"
    | "getNextPageParam"
    | "initialPageParam"
>;

export {
    type PaginatedQuery,
    type PaginatedQueryOptions,
    type PageParameter,
    type Query,
    type QueryOptions,
};
