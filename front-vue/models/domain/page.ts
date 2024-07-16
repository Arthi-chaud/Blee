import * as yup from "yup";

export type PaginationParameters = Partial<{
    take: number;
    skip: number;
}>;

const PaginatedResponse = <T>(itemType: yup.Schema<T>) =>
    yup.object({
        items: yup.array(itemType).required(),
        metadata: yup.object({
            count: yup.number().required(),
            next: yup.string().required().nullable(),
        }),
    });

type PaginatedResponse<T> = yup.InferType<
    ReturnType<typeof PaginatedResponse<T>>
>;

export default PaginatedResponse;
