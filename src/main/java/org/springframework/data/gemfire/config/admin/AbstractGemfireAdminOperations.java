/*
 * Copyright 2017 the original author or authors.
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

package org.springframework.data.gemfire.config.admin;

import org.apache.geode.cache.DiskStore;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.query.Index;
import org.springframework.data.gemfire.config.schema.SchemaObjectDefinition;
import org.springframework.data.gemfire.config.schema.definitions.IndexDefinition;
import org.springframework.data.gemfire.config.schema.definitions.RegionDefinition;

/**
 * {@link AbstractGemfireAdminOperations} is an abstract base class encapsulating common functionality
 * supporting administrative (management) operations against a Pivotal GemFire or Apache Geode cluster.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.admin.GemfireAdminOperations
 * @see org.springframework.data.gemfire.config.schema.SchemaObjectDefinition
 * @since 2.0.0
 */
public class AbstractGemfireAdminOperations implements GemfireAdminOperations {

	protected static final String NOT_IMPLEMENTED = "Not Implemented";

	/**
	 * Returns a {@link Iterable collection} of {@link Region} names defined on the GemFire Servers in the cluster.
	 *
	 * @return an {@link Iterable} of {@link Region} names defined on the GemFire Servers in the cluster.
	 * @see org.apache.geode.cache.Region#getName()
	 * @see java.lang.Iterable
	 */
	@Override
	public Iterable<String> getAvailableServerRegions() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/**
	 * Returns an {@link Iterable} of all the server {@link Region} {@link Index Indexes}.
	 *
	 * @return an {@link Iterable} of all the server {@link Region} {@link Index Indexes}.
	 * @see org.apache.geode.cache.query.Index#getName()
	 * @see java.lang.Iterable
	 */
	@Override
	public Iterable<String> getAvailableServerRegionIndexes() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/**
	 * Creates a cache {@link Region} based on the given {@link RegionDefinition schema object definition}.
	 *
	 * @param regionDefinition {@link RegionDefinition} encapsulating configuration meta-data defining
	 * a cache {@link Region}.
	 * @see org.springframework.data.gemfire.config.schema.definitions.RegionDefinition
	 * @see org.apache.geode.cache.GemFireCache
	 * @see org.apache.geode.cache.Region
	 */
	@Override
	public void createRegion(RegionDefinition regionDefinition) {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/**
	 * Creates a {@link Region} {@link LuceneIndex} based on the given
	 * {@link SchemaObjectDefinition schema object definition}.
	 *
	 * @param luceneIndexDefinition {@link SchemaObjectDefinition} encapsulating the configuration meta-data
	 * defining a {@link Region} {@link LuceneIndex}.
	 * @see org.springframework.data.gemfire.config.schema.SchemaObjectDefinition
	 * @see org.apache.geode.cache.lucene.LuceneIndex
	 * @see org.apache.geode.cache.Region
	 */
	@Override
	public void createLuceneIndex(SchemaObjectDefinition luceneIndexDefinition) {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/**
	 * Creates a {@link Region} OQL {@link Index} based on the given {@link IndexDefinition schema object definition}.
	 *
	 * @param indexDefinition {@link IndexDefinition} encapsulating the configuration meta-data
	 * defining a {@link Region} OQL {@link Index}.
	 * @see org.springframework.data.gemfire.config.schema.definitions.IndexDefinition
	 * @see org.apache.geode.cache.query.Index
	 * @see org.apache.geode.cache.Region
	 */
	@Override
	public void createIndex(IndexDefinition indexDefinition) {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/**
	 * Creates a {@link DiskStore} based on the given {@link SchemaObjectDefinition schema object definition}.
	 *
	 * @param diskStoreDefinition {@link SchemaObjectDefinition} encapsulating the configuration meta-data
	 * defining a {@link DiskStore}.
	 * @see org.springframework.data.gemfire.config.schema.SchemaObjectDefinition
	 * @see org.apache.geode.cache.DiskStore
	 */
	@Override
	public void createDiskStore(SchemaObjectDefinition diskStoreDefinition) {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}
}
