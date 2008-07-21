class CreateNewones < ActiveRecord::Migration
  def self.up
    create_table :newones do |t|

      t.timestamps
    end
  end

  def self.down
    drop_table :newones
  end
end
