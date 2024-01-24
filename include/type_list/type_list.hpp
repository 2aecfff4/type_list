#pragma once
#include <cstdint>
#include <type_traits>
#include <utility>

namespace type_list {

///
template <typename... Ts>
struct TypeList {
    using Type = TypeList;

    [[nodiscard]] static constexpr std::size_t count() noexcept { return sizeof...(Ts); }
};

namespace type_at_private {

///
template <std::size_t idx, typename List>
struct TypeAtImpl;

///
template <typename T, typename... Ts>
struct TypeAtImpl<0, TypeList<T, Ts...>> {
    using Type = T;
};

///
template <std::size_t idx, typename T, typename... Ts>
struct TypeAtImpl<idx, TypeList<T, Ts...>> {
    using Type = typename TypeAtImpl<idx - 1, TypeList<Ts...>>::Type;
};

///
template <std::size_t idx, typename List>
struct TypeAt;

///
template <std::size_t idx, typename... Ts>
struct TypeAt<idx, TypeList<Ts...>> {
    static_assert(sizeof...(Ts) != 0, "`TypeList` is empty!");
    static_assert(sizeof...(Ts) > idx, "Index out of bounds!");
    using Type = typename TypeAtImpl<idx, TypeList<Ts...>>::Type;
};

} // namespace type_at_private

///
template <std::size_t idx, typename... Ts>
using type_at_t = typename type_at_private::TypeAt<idx, Ts...>::Type;

namespace index_of_private {

///
template <typename>
[[nodiscard]]
constexpr std::size_t type_index([[maybe_unused]] std::size_t index) noexcept {
    return static_cast<std::size_t>(-1);
}

///
template <typename IndexedType, typename T, typename... Ts>
[[nodiscard]]
constexpr std::size_t get_type_index(std::size_t index = 0) noexcept {
    if constexpr (std::is_same_v<IndexedType, T>) {
        return index;
    } else {
        return type_index<IndexedType, Ts...>(index + 1);
    }
}

///
template <typename T, typename List>
struct TypeIndexOf;

///
template <typename T, typename... Types>
struct TypeIndexOf<T, TypeList<Types...>>
    : public std::integral_constant<std::size_t, type_index<T, Types...>()> {};

} // namespace index_of_private

///
template <typename T, typename... Types>
constexpr inline std::size_t index_of_v
    = index_of_private::TypeIndexOf<T, Types...>::value;

namespace unique_type_list_private {

///
template <typename T, typename... Ts>
struct UniqueTypeListImpl {
    using type = T;
};

///
template <typename... Ts, typename U, typename... Us>
struct UniqueTypeListImpl<TypeList<Ts...>, U, Us...>
    : public std::conditional_t<
          (std::is_same_v<U, Ts> || ...),
          UniqueTypeListImpl<TypeList<Ts...>, Us...>,
          UniqueTypeListImpl<TypeList<Ts..., U>, Us...>> {};

} // namespace unique_type_list_private

///
template <typename... Ts>
using unique_t =
    typename unique_type_list_private::UniqueTypeListImpl<TypeList<>, Ts...>::type;

namespace rename_private {

///
template <typename A, template <typename...> typename B>
struct RenameImpl;

///
template <template <typename...> typename A, typename... T, template <typename...> typename B>
struct RenameImpl<A<T...>, B> {
    using Type = B<T...>;
};

} // namespace rename_private

///
template <typename A, template <typename...> typename B>
using rename_t = typename rename_private::RenameImpl<A, B>::Type;

namespace overload_resolution_private {

///
template <typename T>
struct Overload {
    using Fn = T (*)(T);
    operator Fn() const noexcept; // NOLINT(hicpp-explicit-conversions)
};

///
template <typename... Ts>
struct OverloadSetImpl;

///
template <typename... Ts>
struct OverloadSetImpl<TypeList<Ts...>> : public Overload<Ts>... {};

///
template <
    typename T,
    typename OverloadSet,
    typename Result = std::invoke_result_t<OverloadSet, T>>
struct OverloadResolution {
    using Type = Result;
};

} // namespace overload_resolution_private

///
template <typename... Ts>
struct OverloadSet
    : public overload_resolution_private::OverloadSetImpl<unique_t<Ts...>> {};

///
template <typename T, typename OverloadSet>
using overload_resolution_t =
    typename overload_resolution_private::OverloadResolution<T, OverloadSet>::Type;

} // namespace type_list
