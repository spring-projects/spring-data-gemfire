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

package org.springframework.data.gemfire.config.schema.definitions;

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Optional;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.query.Index;
import org.springframework.data.gemfire.IndexType;
import org.springframework.data.gemfire.config.admin.GemfireAdminOperations;
import org.springframework.data.gemfire.config.schema.SchemaObjectDefinition;
import org.springframework.data.gemfire.config.schema.SchemaObjectType;
import org.springframework.data.gemfire.domain.support.AbstractIndexSupport;
import org.springframework.util.StringUtils;

/**
 * {@link IndexDefinition} is an Abstract Data Type (ADT) encapsulating the configuration meta-data used to define
 * a new Apache Geode / Pivotal GemFire {@link Region} {@link Index}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.query.Index
 * @see org.springframework.data.gemfire.IndexType
 * @see org.springframework.data.gemfire.config.admin.GemfireAdminOperations
 * @see org.springframework.data.gemfire.config.schema.SchemaObjectDefinition
 * @see org.springframework.data.gemfire.config.schema.SchemaObjectType
 * @since 2.0.0
 */
public class IndexDefinition extends SchemaObjectDefinition {

	protected static final int ORDER = RegionDefinition.ORDER + 1;

	/**
	 * Factory method used to construct a new instance of {@link IndexDefinition} defined from the given {@link Index}.
	 *
	 * @param index {@link Index} on which the new {@link IndexDefinition} will be defined;
	 * must not be {@literal null}.
	 * @return a new instance of {@link IndexDefinition} defined from the given {@link Index}.
	 * @throws IllegalArgumentException if {@link Index} is {@literal null}.
	 * @see org.apache.geode.cache.query.Index
	 */
	public static IndexDefinition from(Index index) {
		return new IndexDefinition(index);
	}

	private transient Index index;

	private IndexType indexType;

	private String expression;
	private String fromClause;
	private String name;

	/**
	 * Constructs a new instance of {@link IndexDefinition} defined with the given {@link Index}.
	 *
	 * @param index {@link Index} on which this definition is defined; must not be {@literal null}.
	 * @throws IllegalArgumentException if {@link Index} is {@literal null}.
	 * @see org.apache.geode.cache.query.Index
	 */
	protected IndexDefinition(Index index) {

		super(Optional.ofNullable(index).map(Index::getName)
			.orElseThrow(() -> newIllegalArgumentException("Index is required")));

		this.index = index;
	}

	/**
	 * Returns a reference to the {@link Index} on which this definition is defined.
	 *
	 * @return a reference to the {@link Index} on which this definition is defined.
	 * @see org.apache.geode.cache.query.Index
	 */
	protected Index getIndex() {
		return this.index;
	}

	@Override
	public String getName() {
		return Optional.ofNullable(this.name).filter(StringUtils::hasText).orElseGet(super::getName);
	}

	public String getExpression() {
		return Optional.ofNullable(this.expression).filter(StringUtils::hasText)
			.orElseGet(this.index::getIndexedExpression);
	}

	public String getFromClause() {
		return Optional.ofNullable(this.fromClause).filter(StringUtils::hasText)
			.orElseGet(this.index::getFromClause);
	}

	public IndexType getIndexType() {
		return Optional.ofNullable(this.indexType).orElseGet(() -> IndexType.valueOf(this.index.getType()));
	}

	/**
	 * Get the order value of this object.
	 *
	 * @return the order value of this object.
	 * @see org.springframework.core.Ordered
	 */
	@Override
	public int getOrder() {
		return ORDER;
	}

	@Override
	public SchemaObjectType getType() {
		return SchemaObjectType.INDEX;
	}

	@Override
	public void create(GemfireAdminOperations gemfireAdminOperations) {
		gemfireAdminOperations.createIndex(this);
	}

	@SuppressWarnings("deprecation")
	public IndexDefinition as(org.apache.geode.cache.query.IndexType gemfireGeodeIndexType) {
		return as(IndexType.valueOf(gemfireGeodeIndexType));
	}

	public IndexDefinition as(IndexType indexType) {
		this.indexType = indexType;
		return this;
	}

	public IndexDefinition having(String expression) {

		this.expression = Optional.ofNullable(expression).filter(StringUtils::hasText)
			.orElseThrow(() -> newIllegalArgumentException("Expression is required"));

		return this;
	}

	public IndexDefinition on(Region<?, ?> region) {
		return on(Optional.ofNullable(region).map(Region::getFullPath)
			.orElseThrow(() -> newIllegalArgumentException("Region is required")));
	}

	public IndexDefinition on(String fromClause) {

		this.fromClause = Optional.ofNullable(fromClause).filter(StringUtils::hasText)
			.orElseThrow(() -> newIllegalArgumentException("From Clause is required"));

		return this;
	}

	public IndexDefinition with(String name) {
		this.name = name;
		return this;
	}

	private void writeObject(ObjectOutputStream outputStream) throws IOException {
		outputStream.writeUTF(getName());
		outputStream.writeUTF(getExpression());
		outputStream.writeUTF(getFromClause());
		outputStream.writeObject(getIndexType());
	}

	private void readObject(ObjectInputStream inputStream) throws ClassNotFoundException, IOException {

		String name = inputStream.readUTF();
		String expression = inputStream.readUTF();
		String fromClause = inputStream.readUTF();

		IndexType indexType = (IndexType) inputStream.readObject();

		this.index = IndexWrapper.from(name, expression, fromClause, indexType);
	}

	protected static final class IndexWrapper extends AbstractIndexSupport {

		private IndexType indexType;

		private final String name;
		private final String expression;
		private final String fromClause;

		protected static Index from(String name, String expression, String fromClause, IndexType indexType) {
			return new IndexWrapper(name, expression, fromClause, indexType);
		}

		protected IndexWrapper(String name, String expression, String fromClause, IndexType indexType) {
			this.name = name;
			this.expression = expression;
			this.fromClause = fromClause;
			this.indexType = indexType;
		}

		@Override
		public String getFromClause() {
			return this.fromClause;
		}

		@Override
		public String getIndexedExpression() {
			return this.expression;
		}

		@Override
		public String getName() {
			return this.name;
		}

		@Override
		@SuppressWarnings("deprecation")
		public org.apache.geode.cache.query.IndexType getType() {
			return Optional.ofNullable(this.indexType).map(IndexType::getGemfireIndexType).orElse(null);
		}
	}
}
