/*
 * Copyright 2017-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
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
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.data.gemfire.config.schema.SchemaObjectDefiner;
import org.springframework.data.gemfire.config.schema.SchemaObjectDefinition;
import org.springframework.data.gemfire.config.schema.SchemaObjectType;
import org.springframework.lang.Nullable;

/**
 * {@link ComposableSchemaObjectDefiner} is an implementation of {@link SchemaObjectDefiner}
 * as well as a composition of {@link SchemaObjectDefiner SchemaObjectInstanceHandlers} composed
 * using the Composite Software Design Pattern.
 *
 * @author John Blum
 * @see java.lang.Iterable
 * @see org.springframework.data.gemfire.config.schema.SchemaObjectDefiner
 * @since 2.0.0
 */
public final class ComposableSchemaObjectDefiner
		implements SchemaObjectDefiner, Iterable<SchemaObjectDefiner> {

	@Nullable
	public static SchemaObjectDefiner compose(SchemaObjectDefiner... schemaObjectDefiners) {
		return compose(Arrays.asList(nullSafeArray(schemaObjectDefiners, SchemaObjectDefiner.class)));
	}

	@Nullable
	public static SchemaObjectDefiner compose(Iterable<SchemaObjectDefiner> schemaObjectDefiners) {

		Set<SchemaObjectDefiner> schemaObjectDefinerSet =
			stream(nullSafeIterable(schemaObjectDefiners).spliterator(), false)
				.filter(Objects::nonNull).collect(Collectors.toSet());

		return (schemaObjectDefinerSet.isEmpty() ? null
			: (schemaObjectDefinerSet.size() == 1 ? schemaObjectDefinerSet.iterator().next()
			: new ComposableSchemaObjectDefiner(schemaObjectDefinerSet)));
	}

	private final Set<SchemaObjectDefiner> schemaObjectDefiners;

	/**
	 * Constructs a new instance of {@link ComposableSchemaObjectDefiner} initialized and compose of
	 * the given {@link Set} of {@link SchemaObjectDefiner SchemaObjectInstanceHandlers}.
	 *
	 * @param schemaObjectDefiners {@link Set} of {@link SchemaObjectDefiner SchemaObjectInstanceHandlers}
	 * from which this {@link ComposableSchemaObjectDefiner} is composed.
	 * @see SchemaObjectDefiner
	 */
	private ComposableSchemaObjectDefiner(Set<SchemaObjectDefiner> schemaObjectDefiners) {
		this.schemaObjectDefiners = Collections.unmodifiableSet(schemaObjectDefiners);
	}

	@Override
	public Set<SchemaObjectType> getSchemaObjectTypes() {
		return this.schemaObjectDefiners.stream()
			.flatMap(it -> it.getSchemaObjectTypes().stream())
			.collect(Collectors.toSet());
	}

	@Override
	public Optional<? extends SchemaObjectDefinition> define(Object schemaObject) {

		return this.schemaObjectDefiners.stream()
			.filter(it -> it.canDefine(schemaObject)).findAny()
			.map(it -> it.define(schemaObject).orElse(null));
	}

	@Override
	public Iterator<SchemaObjectDefiner> iterator() {
		return Collections.unmodifiableSet(this.schemaObjectDefiners).iterator();
	}
}
