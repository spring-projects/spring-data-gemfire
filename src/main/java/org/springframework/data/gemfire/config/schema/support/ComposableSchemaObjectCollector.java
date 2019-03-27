/*
 * Copyright 2017-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config.schema.support;

import static java.util.stream.StreamSupport.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;

import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.geode.cache.GemFireCache;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.config.schema.SchemaObjectCollector;
import org.springframework.lang.Nullable;

/**
 * The {@link ComposableSchemaObjectCollector} class is a {@link SchemaObjectCollector} implementation composed of
 * multiple {@link SchemaObjectCollector} objects wrapped in a facade and treated like a single
 * {@link SchemaObjectCollector} using the Composite Software Design Pattern.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.schema.SchemaObjectCollector
 * @since 2.0.0
 */
public final class ComposableSchemaObjectCollector
		implements SchemaObjectCollector<Object>, Iterable<SchemaObjectCollector<?>> {

	private final List<SchemaObjectCollector<?>> schemaObjectCollectors;

	@Nullable
	@SuppressWarnings("unchecked")
	public static SchemaObjectCollector<?> compose(SchemaObjectCollector<?>... schemaObjectCollectors) {
		return compose(Arrays.asList(nullSafeArray(schemaObjectCollectors, SchemaObjectCollector.class)));
	}

	@Nullable
	public static SchemaObjectCollector<?> compose(Iterable<SchemaObjectCollector<?>> schemaObjectCollectors) {

		List<SchemaObjectCollector<?>> schemaObjectCollectorList =
			stream(nullSafeIterable(schemaObjectCollectors).spliterator(), false)
				.filter(Objects::nonNull).collect(Collectors.toList());

		return (schemaObjectCollectorList.isEmpty() ? null
			: (schemaObjectCollectorList.size() == 1 ? schemaObjectCollectorList.iterator().next()
				: new ComposableSchemaObjectCollector(schemaObjectCollectorList)));
	}

	private ComposableSchemaObjectCollector(List<SchemaObjectCollector<?>> schemaObjectCollectors) {
		this.schemaObjectCollectors = Collections.unmodifiableList(schemaObjectCollectors);
	}

	@Override
	public Iterable<Object> collectFrom(ApplicationContext applicationContext) {
		return collectFrom(collector -> collector.collectFrom(applicationContext));
	}

	@Override
	public Iterable<Object> collectFrom(GemFireCache gemfireCache) {
		return collectFrom(collector -> collector.collectFrom(gemfireCache));
	}

	private Iterable<Object> collectFrom(Function<SchemaObjectCollector<?>, Iterable<?>> schemaObjectSource) {

		return this.schemaObjectCollectors.stream()
			.flatMap(collector -> stream(schemaObjectSource.apply(collector).spliterator(), false))
			.collect(Collectors.toList());
	}

	@Override
	public Iterator<SchemaObjectCollector<?>> iterator() {
		return Collections.unmodifiableList(this.schemaObjectCollectors).iterator();
	}
}
