/*
 * Copyright 2017-2020 the original author or authors.
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

import static org.springframework.data.gemfire.util.CollectionUtils.asSet;

import java.util.Optional;
import java.util.Set;

import org.apache.geode.cache.query.Index;
import org.springframework.data.gemfire.config.schema.SchemaObjectDefiner;
import org.springframework.data.gemfire.config.schema.SchemaObjectType;
import org.springframework.data.gemfire.config.schema.definitions.IndexDefinition;

/**
 * The {@link {{@link IndexDefiner }} class is responsible for defining an {@link Index} given a reference to
 * an {@link Index} instance.
 *
 * @author John Blum
 * @see org.apache.geode.cache.query.Index
 * @see org.springframework.data.gemfire.config.schema.SchemaObjectDefiner
 * @see org.springframework.data.gemfire.config.schema.SchemaObjectType
 * @see org.springframework.data.gemfire.config.schema.definitions.IndexDefinition
 * @since 2.0.0
 */
public class IndexDefiner implements SchemaObjectDefiner {

	@Override
	public Set<SchemaObjectType> getSchemaObjectTypes() {
		return asSet(SchemaObjectType.INDEX);
	}

	@Override
	public Optional<IndexDefinition> define(Object schemaObject) {
		return Optional.ofNullable(schemaObject).filter(this::canDefine).map(it -> IndexDefinition.from((Index) it));
	}
}
